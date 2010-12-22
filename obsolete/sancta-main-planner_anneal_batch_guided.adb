with Sancta.Config;
with Sancta.Methods.Choose_Entry_Point;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Planner_Anneal;
with Sancta.Planner_Central;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Explore_Segment;
with Sancta.Types; use Sancta.Types;

pragma Warnings (Off);
with Sancta.Tasks.Used;
pragma Warnings (On);

with Agpl.Chronos;
with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Gdk.Managed;
with Sancta.Method;
with Sancta.Tasks;
with Agpl.Optimization.Annealing;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Sancta.Main.Planner_Anneal_Batch_Guided is

   Our_Id : aliased Network.Node_Id;

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

   This   : Sancta.Planner_Anneal.Object (Link'Access);

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      --  Orderly shut down:
      Link.Shutdown;
      Gdk.Managed.Shutdown;

      --  Finally, forced shut down:
      delay 1.0;
      Gnat.Os_Lib.Os_Exit (-1);
   exception
      when E : others =>
         Log ("Shutting down: " & Report (E), Warning);
         Gnat.Os_Lib.Os_Exit (-1);
   end Shutdown;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage:");
      Put_Line (Program_Name & " --id <node_id> --config <file> [OPTIONS]");
      Put_Line ("   No options defined for now.");
   end Usage;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks is
   begin
      declare
         E1 : array (1 .. 2) of Tasks.Choose_Entry_Point.Object :=
               (others =>
                      Tasks.Choose_Entry_Point.Create
                  ((1 => (0.0, 58.0, -3.14159265))));
         E2 : array (1 .. 2) of Tasks.Choose_Entry_Point.Object :=
               (others =>
                      Tasks.Choose_Entry_Point.Create
                  ((1 => (0.0,-58.0, 3.1415926535))));
      begin
         --  Send these jobs:
         for I in E1'Range loop
            This.Add_Task (E1 (I));
         end loop;
         for I in E2'Range loop
            This.Add_Task (E2 (I));
         end loop;
      end;

      --  Create the segments to explore:
      --  Angles are ignored to compute cost of these tasks.
      declare
         S  : array (1 .. 32) of Tasks.Explore_Segment.Object;
         X  : Types.Real := -50.0;
      begin
         for I in 1 .. 16 loop
            S (I) := Tasks.Explore_Segment.Create (( -6.0, X, 0.0),
                                                   (-75.0, X, 0.0));
            S (I + 16) := Tasks.Explore_Segment.Create (( 6.0, X, 0.0),
                                                        (75.0, X, 0.0));
            X := X + (100.0 / Types.Real (S'Length / 2 - 1));
         end loop;

         --  Send these jobs:
         for I in S'Range loop
            This.Add_Task (S (I));
         end loop;

         Log ("STARTING RUN ****************************************", Always);

      end;
   end Add_Tasks;

begin

   if not Exists ("--id") or else not Exists ("--config") then
      Usage;
      return;
   end if;

   Gdk.Managed.Start;

   --  Common configuration.
   Config.Init (Link => Link'Unrestricted_Access, Log_Level => Debug);
--   Config.Init (Log_Level => Debug);
   Our_Id := Config.Node_Id;

   Enable_Section (Trace.Get_Default_Tracer.all, Agpl.Optimization.Annealing.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Agpl.Optimization.Annealing.Detail_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Central.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Central.Detail_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Anneal.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Anneal.Detail_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Plan.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Plan.Detail_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Network.Layer.Udp.Log_Section);

   --  Set up comms.
   Network.Groups.Init (Config.Options);
   Link.Init (Config.Options);


   --  Config planner
   This.Add_Method (Methods.Explore_Segment_Expansion.Object'(Sancta.Method.Object
                                                              with null record));
   This.Add_Method (Methods.Choose_Entry_Point.Object'(Sancta.Method.Object
                                                              with null record));
   This.Set_Configuration (Convergence_Period => Duration'Last);

   --  Force new tasks being created here to have unique numbering!!
   --  Since the ones received via network come from another series.
   --  Really, the unique id of a task should come from the task description.
   --  What a silly mess.
   Sancta.Tasks.Set_Next_Id (1001);

   Log ("Annealing planner ready.", Informative);

   --  Main loop
   declare
      Done : Boolean := False;
      Wait : Chronos.Object;
   begin
      while not Done loop
         delay 0.01;

         This.Run (Done);

         if Wait.Elapsed > 2.0 then
            Wait.Reset;
            Add_Tasks;
            delay 2.0;
            This.Run (Done);
            delay 10.0; -- To see solution
            Shutdown;
         end if;
      end loop;
   end;

exception
   when E : others =>
      Log ("Planner_Anneal [Main]: " & Report (E), Error);
      Shutdown;


end Sancta.Main.Planner_Anneal_Batch_Guided;
