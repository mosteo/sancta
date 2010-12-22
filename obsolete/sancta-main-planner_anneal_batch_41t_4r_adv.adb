with Sancta.Config;
with Sancta.Methods.Choose_Entry_Point;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Planner_Anneal;
with Sancta.Planner_Central;
with Sancta.Problems.City43;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Types;

pragma Warnings (Off);
with Sancta.Tasks.Used;
pragma Warnings (On);

with Agpl.Chronos;
with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Gdk.Managed;
with Sancta.Method;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Sancta.Main.Planner_Anneal_Batch_41t_4r_Adv is

   Num_Robots : constant := 4;

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
      V : constant Sancta.Tasks.Containers.Vector := Problems.City43.Get_Tasks;
   begin
      --  Segments
      for I in V.First_Index .. V.Last_Index loop
         This.Add_Task (V.Element (I));
      end loop;

      --  Entry points
      declare
         use type Types.Real;
         Entries : array (1 .. 4) of Tasks.Choose_Entry_Point.Object;
      begin
         Entries (1) := Tasks.Choose_Entry_Point.Create ((1 => ( 21.0,  0.0,  3.1)));
         Entries (2) := Tasks.Choose_Entry_Point.Create ((1 => (-24.0, -6.0,  0.0)));
         Entries (3) := Tasks.Choose_Entry_Point.Create ((1 => (-24.0, -6.0,  0.0)));
         Entries (4) := Tasks.Choose_Entry_Point.Create ((1 => ( 21.0,-18.0,  2.0)));

         for I in Entries'Range loop
            This.Add_Task (Entries (I));
         end loop;
      end;

      Log ("STARTING RUN ****************************************", Always);
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


end Sancta.Main.Planner_Anneal_Batch_41t_4r_Adv;
