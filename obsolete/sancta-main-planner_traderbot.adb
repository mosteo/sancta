with Sancta.Auctioner;
with Sancta.Config;
with Sancta.Network;
with Sancta.Network.Layer.Udp;
with Sancta.Planner_Traderbot;

pragma Warnings (Off);
with Sancta.Tasks.Used;
pragma Warnings (On);

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Xml;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Sancta.Main.Planner_Traderbot is

   Our_Id : aliased Network.Node_Id;

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

   This   : Sancta.Planner_Traderbot.Object (Link'Access);

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
      Put_Line ("   No options defined for now.");
   end Usage;

begin

   if not Exists ("--id") or else not Exists ("--config") then
      Usage;
      return;
   end if;

   --  Common configuration.
   Config.Init (Log_Level => Debug);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Auctioner.Log_Section);
   Enable_Section (Trace.Get_Default_Tracer.all, Sancta.Planner_Traderbot.Log_Section);
--   Enable_Section (Config.Tracer, Sancta.Network.Layer.Udp.Log_Section);

   Our_Id := Config.Node_Id;

   --  Set up comms.
   Link.Init (Config.Options);

   --  Set up auction options.
   This.Set_Configuration
     (Config.Options,
      Cost_Policy => Auctioner.Cost_Policies'Value
        (Xml.Get_Attribute ("traderbot", "cost_policy",
                            Config.Options, "full_cost")));

   Log ("Planner traderbot ready", Always);

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
      Log ("Traderbot [Main]: " & Report (E), Error);
      Shutdown;


end Sancta.Main.Planner_Traderbot;
