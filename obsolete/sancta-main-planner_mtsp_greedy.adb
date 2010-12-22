with Sancta.Assigners.Greedy;
with Sancta.Config;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Planner_Central;
with Sancta.Planner_Htn;

pragma Warnings (Off);
with Sancta.Tasks.Used;
pragma Warnings (On);

with Agpl.Command_Line; use Agpl.Command_Line;
with Sancta;
with Sancta.Assigner.Hungry2;
with Sancta.Assigner.Hungry3;
with Agpl.Gdk.Managed;
with Sancta.Method;
with Sancta.Plan;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Sancta.Main.Planner_Mtsp_Greedy is

   Our_Id : aliased Network.Node_Id;

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

   This   : Sancta.Planner_Sancta.Object (Link'Access);
   Ass    : Sancta.Assigner.Hungry2.Object;
   Ass2   : Sancta.Assigners.Greedy.Object;
   Ass3   : Sancta.Assigner.Hungry3.Object;

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

   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Central.Log_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Central.Detail_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Sancta.Log_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Sancta.Detail_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Plan.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Plan.Detail_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Network.Layer.Udp.Log_Section);

   --  Set up comms.
   Network.Groups.Init (Config.Options);
   Link.Init (Config.Options);


   --  Config planner
   This.Set_Configuration (Assigner  => Ass3,
                           Criterion => Sancta.Minimax);
   This.Add_Method (Methods.Explore_Segment_Expansion.Object'(Sancta.Method.Object
                                                              with null record));

   Log ("MTSP planner ready.", Informative);

   --  Main loop
   declare
      Done : Boolean := False;
   begin
      while not Done loop
         delay 0.01;

         This.Run (Done);
      end loop;
   end;

   Shutdown;

exception
   when E : others =>
      Log ("Planner_MTSP [Main]: " & Report (E), Error);
      Shutdown;


end Sancta.Main.Planner_Mtsp_Greedy;
