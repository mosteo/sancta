with Sancta.Config;
--  with Sancta.Debug2;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Planner_Anneal;
with Sancta.Planner_Central;
with Sancta.Problems.City43;

pragma Warnings (Off);
with Sancta.Tasks.Used;
pragma Warnings (On);

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Gdk.Managed;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Sancta.Main.Ficticio_41t is

   Our_Id : aliased Network.Node_Id;

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

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
         V : constant Sancta.Tasks.Containers.Vector := Problems.City43.Get_Tasks;
         use Sancta.Tasks.Containers;
      begin
         Log ("Sending tasks...", Always);
         for I in First_Index (V) .. Last_Index (V) loop
            Log ("Sending: " & Element (V, I).To_String, Always);
            Network.Layer.Udp.Multicast
              (Link,
               Network.Groups.Management_Channel,
               Network.Messages.Propose_Task (Element (V, I)));
         end loop;
         New_Line;
         Log ("Done", Always);
      end;
   end Add_Tasks;

begin

   if not Exists ("--id") or else not Exists ("--config") then
      Usage;
      return;
   end if;

--   Gdk.Managed.Start;

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

   Add_Tasks;

   Link.Shutdown;

--   Gdk.Managed.Shutdown;

exception
   when E : others =>
      Log ("Planner_Anneal [Main]: " & Report (E), Error);
      Shutdown;


end Sancta.Main.Ficticio_41t;
