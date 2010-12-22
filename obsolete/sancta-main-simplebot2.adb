with Sancta.Config;
with Sancta.Gui.Robot_Network_Update;
with Sancta.Netbot;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Robot;
with Sancta.Simplebot;
with Sancta.Simplebot2;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;
with Agpl.Xml;          use Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Text_Io; use Text_Io;

--  A simplebot just says HELLOs, receives tasks and carries them out.
--  From netbot inherits the DONE messages.

--  A simplebot2 additionally broadcasts its pose every second, too.

procedure Sancta.Main.Simplebot2 is

   Our_Id : aliased Node_Id;

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);
   Agent  : aliased Robot.Object;
   Relay  :         Gui.Robot_Network_Update.Object (Link'Access);

   Simple : aliased Sancta.Simplebot2.Object (Agent'Access, Link'Access);

   Bot    : constant Netbot.Object_Access := Simple'Unrestricted_Access;

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
   Config.Init (Link => Link'Unrestricted_Access, Log_Level => Debug);
--   Config.Init (Log_Level => Debug);

   Trace.Console_Tracer.Enable_Section (Netbot.Log_Section);
   Trace.Console_Tracer.Enable_Section (Sancta.Simplebot2.Log_Section);
   Trace.Console_Tracer.Enable_Section (Sancta.Simplebot.Log_Section);
--   Enable_Section (Trace.Get_Default_Tracer.all, Network.Layer.Udp.Log_Section);

   Our_Id := Config.Node_Id;

   --  Set up comms.
   Network.Groups.Init (Config.Options);
   Link.Init (Config.Options);

   --  Set up robot.
   Agent := Sancta.Robot.Create (Network.Image (Config.Node_Id));
   Agent.Add_Listener2 (Relay);

--   Bot.Set_Configuration (Config.Options);

   Log ("Simplebot2 initialized OK.", Always);

   --  Main loop
   declare
      Done : Boolean := False;
   begin
      while not Done loop
         delay 0.01;

         Bot.Run (Done);
      end loop;
   end;

   Agent.Emergency_Stop;
   Shutdown;

exception
   when E : others =>
      Log ("Simplebot2 [Main]: " & Report (E), Error);
      Shutdown;
end Sancta.Main.Simplebot2;
