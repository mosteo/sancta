with Ada.Calendar; use Ada.Calendar;
with Agpl.Chronos;
with Agpl.Containers.String_Float_Maps;
with Agpl.Event_Queues;
with Agpl.Event_Queues.Calendar; pragma Elaborate_All (Agpl.Event_Queues.Calendar);
with Agpl.Tasking.Code;
with Agpl.Tasking.Workers;
with Agpl; use Agpl;
with Sancta.Options;

package body Sancta.Events is

   Queue : Event_Queues.Calendar.Object
     (Stack_Size => 4 * 1024 * 1024);

   type Plugin_Context is new Event_Queues.Context_Type with record
      Component : Sancta.Component.Object_Access;
   end record;

   Startup_Time : Chronos.Object;

   -------------
   -- Cpu_Use --
   -------------

   protected Cpu_Use is
      function Get (Context : String) return Float;
      procedure Add (Context : String; Val : Float);
   private
      Counter : Agpl.Containers.String_Float_Maps.Map;
   end Cpu_Use;

   -------------
   -- Cpu_Use --
   -------------

   protected body Cpu_Use is
      function Get (Context : String) return Float is
         use Agpl.Containers.String_Float_Maps;
         I : constant Cursor := Counter.Find (Context);
      begin
         if Has_Element (I) then
            return Element (I);
         else
            return 0.0;
         end if;
      end Get;

      procedure Add (Context : String; Val : Float) is
      begin
         Counter.Include (Context, Get (Context) + Val);
      end Add;
   end Cpu_Use;

   procedure Do_Call (C : Sancta.Component.Object_Access);
   --  Makes the actual call...

   type Runner is new Agpl.Tasking.Code.Object with record
      C : Sancta.Component.Object_Access;
   end record;

   overriding
   procedure Run (This : in out Runner);
   procedure Run (This : in out Runner) is
   begin
      Do_Call (This.C);
   end Run;

   procedure Call_Plugin (C : Event_Queues.Context_Type'Class);
   --  Choses between single-thread and multi-thread execution...

   -------------
   -- Do_Call --
   -------------

   procedure Do_Call (C : in Sancta.Component.Object_Access) is
      Event  : Event_Queues.Calendar.Event_Type;
      use Agpl.Event_Queues;
      Component : Sancta.Component.Object_Access renames C;
      Next   : Time := Clock + 0.1; -- Unnecesary; safety value.
      Now       : Ada.Calendar.Time;
      Cron      : Chronos.Object;
   begin
      begin
         Log ("Run: " & Component.Name.all, Debug, Log_Section);

         select
            delay Options.Max_Component_Running_Time_Per_Call.Value;
            Log ("Component run [" & Component.Name.all &
                 "] ABORTED after " & Cron.Image,
                 Warning, Log_Section);
         then abort
            Component.Outer_Run (Next);
            if Cron.Elapsed >= 0.2 then
               Log ("Component run [" & Component.Name.all &
                    "] took " & Cron.Image,
                    Debug, Section => Log_Section);
            end if;
         end select;

         if Cron.Elapsed > 1.0 then
            Log ("Component run [" & Component.Name.all & "] " &
                 "took: " & Cron.Image,
                 Warning, Section => Log_Section);
            Log ("Component run [" & Component.Name.all & "] " &
                 "Acum: " & Duration'Image
                   (Duration (Cpu_Use.Get (Component.Name.all))),
                 Warning, Log_Section);
         end if;

         Now := Clock;

         Cpu_Use.Add (Component.Name.all, Float (Cron.Elapsed));

         Log ("Ret: " & Component.Name.all, Debug, Log_Section);
         Log ("Dur: " & Duration'Image (Cron.Elapsed), Debug, Log_Section);
         Log ("Next:" & Duration'Image (Next - Now), Debug, Log_Section);
         Log ("Acum:" & Duration'Image
              (Duration (Cpu_Use.Get (Component.Name.all))),
              Debug, Log_Section);
         if Next < Now and then Startup_Time.Elapsed > 10.0 then
            Log ("Cannot keep up with component: " & Component.Name.all,
                 Warning, Log_Section);
         end if;
      exception
         when E : others =>
            Log ("Call_Plugin [" & Component.Name.all & "]: " & Report (E),
                 Warning, Log_Section);
            Next := Clock + 1.0;
      end;

      Event_Queues.Calendar.Create (Queue,
                                    Event,
                                    Next,
                                    Call_Plugin'Access,
                                    Plugin_Context'(Component => Component));
   end Do_Call;

   -----------------
   -- Call_Plugin --
   -----------------

   procedure Call_Plugin (C : in Event_Queues.Context_Type'Class) is
   begin
      if Plugin_Context (C).Component.Is_Thread_Safe then
         Agpl.Tasking.Workers.Launch
           (Runner'(C => Plugin_Context (C).Component),
            "sancta.event.plugin_call");
      else
         Do_Call (Plugin_Context (C).Component);
      end if;
   end Call_Plugin;

   ------------------
   -- Start_Plugin --
   ------------------

   procedure Start_Plugin (X : in Component.Object_Access) is
      Event : Event_Queues.Calendar.Event_Type;
   begin
      Event_Queues.Calendar.Create (Queue,
                                    Event,
                                    Clock, -- Now!
                                    Call_Plugin'Access,
                                    Plugin_Context'(Component => X));
   end Start_Plugin;

   ------------------
   -- Start_Events --
   ------------------

   procedure Start_Events is
   begin
      Event_Queues.Calendar.Resume (Queue);
   end Start_Events;

   ------------------
   -- Stop_Plugins --
   ------------------

   procedure Stop_Plugins is
   begin
      Event_Queues.Calendar.Shutdown (Queue);
   end Stop_Plugins;

begin
   Event_Queues.Calendar.Suspend (Queue);
end Sancta.Events;
