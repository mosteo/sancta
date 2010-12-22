with Sancta.Config;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;
with Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Ada.Calendar;              use Ada.Calendar;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Text_Io; use Text_Io;

--  MAIN CONTROL CONSOLE

procedure Sancta.Main.Randomwalk_Generator is

   Our_Id : aliased Network.Node_Id;
   --  Zen is the node id for the control station.

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

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
      Put_Line (Program_Name & " --id <Node_Id> --config <file> [OPTIONS]");
      Put_Line ("   No options defined for now.");
   end Usage;

   Period : Duration;
   Xrange : Types.Real;
   Yrange : Types.Real;

   Timer : Time := Clock - 999.0;
   Seed  : Generator;

begin

   if not Exists ("--config") or else not Exists ("--id") then
      Usage;
      return;
   end if;

   --  Common configuration.
   Config.Init (Log_Level => Debug);
   --  Enable_Section (Config.Tracer, Sancta.Network.Layer.Udp.Log_Section);

   Our_Id := Config.Node_Id;

   Network.Groups.Init (Config.Options);

   Period := Duration'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "period"));

   Xrange := Types.Real'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "xrange"));

   Yrange := Types.Real'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "yrange"));

   Reset (Seed);

   --  Set up comms.
   Link.Init (Config.Options);

   --  Main loop
   loop
      delay 0.01;

      --  Create alert
      if Clock - Timer >= Period then
         Timer := Clock;
         declare
            use Sancta.Types;
            Job : Tasks.Goto_Pose.Object;
         begin
            Job.Set_Pose ((Real (Random (Seed) * 2.0 - 1.0) * Xrange,
                           Real (Random (Seed) * 2.0 - 1.0) * Yrange,
                           Angle (Random (Seed) * 2.0 - 1.0) * 3.1415926535));

            Log ("Generated: " & Job.To_String, Informative);

            --  Send the task to some listening manager
            Link.Multicast (Network.Groups.Management_Channel,
                            Network.Messages.Propose_Task (Job));
         end;
      end if;
   end loop;

exception
   when E : others =>
      Log ("Randomwalk [Main]: " & Report (E), Error);
      Shutdown;
end Sancta.Main.Randomwalk_Generator;
