with Sancta.Config;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Strings.Fields;
with Agpl.Trace;        use Agpl.Trace;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Text_Io; use Text_Io;

--  MAIN CONTROL CONSOLE

procedure Sancta.Main.Console is

   Our_Id : aliased Network.Node_Id := Network.Value ("Zen");
   --  Zen is the node id for the control station.

   Link : aliased Network.Layer.Udp.Object (Our_Id'Access);

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
      Put_Line (Program_Name & " --config <file> [OPTIONS]");
      Put_Line ("   No options defined for now.");
   end Usage;

begin

   if not Exists ("--config") then
      Usage;
      return;
   end if;

   --  Common configuration.
   Config.Init (Our_Id, Debug);

   Network.Groups.Init (Config.Options);

   --  Set up comms.
   Link.Init (Config.Options);

   --  Main loop
   loop
      Put_Line ("Enter command:");
      Put_Line ("   shutdown");
      Put_Line ("   goto agent X Y Z");
      declare
         Line : String (1 .. 100);
         Last : Natural;
      begin
         Put ("> ");
         Get_Line (Line, Last);
         declare
            Command : String renames Line (1 .. Last);
            --  Command : constant String := "goto ben 0 0 0";
            use Strings.Fields;
            First : constant String := Select_Field (Command, 1);
         begin
            if First = "shutdown" then
               Link.Multicast (Network.Groups.Emergency_Channel,
                               Network.Messages.Shutdown);
            elsif First = "goto" then
               declare
                  Agent : constant Network.Node_id :=
                            Network.Value (Select_Field (Command, 2));
                  X     : constant Types.Real :=
                            Types.Real'Value (Select_Field (Command, 3));
                  Y     : constant Types.Real :=
                            Types.Real'Value (Select_Field (Command, 4));
                  A     : constant Types.Angle :=
                            Types.Angle'Value (Select_Field (Command, 5));
                  Job   : Tasks.Goto_Pose.Object;
               begin
                  Job.Set_Pose ((X, Y, A));
                  Link.Send (Agent, Network.Messages.Add_Task (Job));
               end;
            else
               Put_Line ("Unrecognized command.");
            end if;
         end;
      exception
         when others =>
            Put_Line ("Invalid data");
      end;
   end loop;

exception
   when E : others =>
      Log ("Console [Main]: " & Report (E), Error);
      Shutdown;
end Sancta.Main.Console;
