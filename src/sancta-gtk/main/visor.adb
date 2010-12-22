with Sancta.Component,
     Sancta.Component.Network,
     Sancta.Component.Redirect,
     Sancta.Config,
     Sancta.Gui,
     Sancta.Gui.Visor,
     Sancta.Gui.Visor.Trace,
     Sancta.Network.Layer,
     Sancta.Starter,
     Sancta.Types;

use Sancta;

with Sancta.Gui.Visor_Db_View;
with Sancta.Gui.Visor_Dbv_View;
with Sancta.Gui.Visor_General_View;
with Sancta.Gui.Visor_Log_View,
     Sancta.Gui.Visor_Mission;

with Agpl.Command_Line; use Agpl.Command_Line;
pragma Warnings (Off);
with Agpl.Task_Termination;
pragma Warnings (On);
with Agpl.Trace;        use Agpl.Trace;
with Agpl; use Agpl;

with Gtk.Main;

with Gnat.Os_Lib;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_Io; use Ada.Text_Io;

procedure Visor is

   use type Types.Real;

   Link   : Sancta.Network.Layer.Object_Access;

   Cycle  : constant Duration := 0.010; -- 10ms cycle time
   Next   : Time := Clock;

   Config : Sancta.Config.Object;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      --  Orderly shut down:
      Link.Shutdown;

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
   end Usage;

   task Main is
      pragma Storage_Size (1024 * 1024);
   end Main;

   ----------
   -- Main --
   ----------

   task body Main is
   begin
      if not Exists ("--id") or else not Exists ("--config") then
         Usage;
         Gnat.Os_Lib.Os_Exit (-1);
      end if;

      --  Registrations
      Sancta.Component.Network.Register;
      Sancta.Component.Redirect.Register;

      Sancta.Gui.Visor_Db_View.Register;
      Sancta.Gui.Visor_Dbv_View.Register;
      Sancta.Gui.Visor_General_View.Register;
      Sancta.Gui.Visor_Log_View.Register;
      Sancta.Gui.Visor_Mission.Register;

      --  Gdk init
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      --  SANCTA startup: plug-in creation
      Sancta.Starter.Launch (Config);

      Link :=
        Sancta.Component.Network.Object
          (Config.Get_Component (Sancta.Component.Network.Name).all).Link;

      declare
         Visor  : aliased Gui.Visor.Object (Link);
         --  Needed so the Gtk initialization happens before elaboration of this object.

         Tracer : aliased Gui.Visor.Trace.Object (Visor'Access);
      begin
         Tracer.Set_Level (Debug);
         Trace.Add_Tracer (Tracer'Unchecked_Access);

         Visor.Create_Simple (Config);

         Log ("Visor ready", Informative);

         --  Main loop
         while not Visor.Is_Done loop
            Next := Next + Cycle;
            delay until Next;

            while Gtk.Main.Events_Pending loop
               if Gtk.Main.Main_Iteration then
                  null;
               end if;
            end loop;

            Visor.Run;
         end loop;
      end;

      Put_Line ("Visor exiting...");

      Config.Destroy_Plugins;
      Shutdown;


   exception
      when E : others =>
         Put_Line ("Visor [Main]: " & Report (E));
         Log ("Visor [Main]: " & Report (E), Error);
         Shutdown;
   end Main;

begin
   null;
exception
   when E : others =>
      Put_Line ("Visor [Environment task]: " & Report (E));
end Visor;
