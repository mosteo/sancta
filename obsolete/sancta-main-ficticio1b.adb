with Sancta.Config;
with Sancta.Debug2;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Tasks.Explore_Segment;
with Sancta.Types; use Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;

with Ada.Text_Io; use Ada.Text_Io;

procedure Sancta.Main.Ficticio1b is

   --  There are three segments to explore to visit all cars.
   --  We'll propose these three compound tasks.
   --  These must be received by the planner (initially the TSP one).
   --  All plans should be computed, the best one chosen, and the tasks
   --  propagated to the robots.

   Our_Id : aliased Node_Id;
   --  We won't listen to messages, it is used simply to be able to transmit.

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

   Split  : constant := 12;

begin
   if not Exists ("--config") or else not Exists ("--id") then
      Put_Line ("<program> --config <file> --id <node id>");
      return;
   end if;

   --  Common configuration.
   Config.Init (Link => Link'Unrestricted_Access, Log_Level => Debug);

   Our_Id := Config.Node_Id;

   Link.Init (Config.Options);
   Network.Groups.Init (Config.Options);

   --  Create the three segments to explore:
   --  Angles are ignored to compute cost of these tasks.
   declare
      S  : array (1 .. 12) of Tasks.Explore_Segment.Object;
   begin
      S (1) := Tasks.Explore_Segment.Create ((-05.0, -50.0, 0.0),
                                             (-75.0, -50.0, 0.0));
      S (3) := Tasks.Explore_Segment.Create ((-06.0, -31.0, 0.0),
                                             (-75.0, -31.0, 0.0));
      S (5) := Tasks.Explore_Segment.Create ((-06.0, -11.5, 0.0),
                                             (-75.0, -11.5, 0.0));
      S (7) := Tasks.Explore_Segment.Create ((-06.0,  08.0, 0.0),
                                             (-75.0,  08.0, 0.0));
      S (9) := Tasks.Explore_Segment.Create ((-06.0,  29.0, 0.0),
                                             (-75.0,  29.0, 0.0));
      S (11) := Tasks.Explore_Segment.Create ((-06.0,  50.0, 0.0),
                                             (-75.0,  50.0, 0.0));

      S (2) := Tasks.Explore_Segment.Create ((06.0, -50.0, 0.0),
                                             (75.0, -50.0, 0.0));
      S (4) := Tasks.Explore_Segment.Create ((06.0, -31.0, 0.0),
                                             (75.0, -31.0, 0.0));
      S (6) := Tasks.Explore_Segment.Create ((06.0, -11.5, 0.0),
                                             (75.0, -11.5, 0.0));
      S (8) := Tasks.Explore_Segment.Create ((06.0,  08.0, 0.0),
                                             (75.0,  08.0, 0.0));
      S (10) := Tasks.Explore_Segment.Create ((06.0,  29.0, 0.0),
                                             (75.0,  29.0, 0.0));
      S (12) := Tasks.Explore_Segment.Create ((06.0,  50.0, 0.0),
                                             (75.0,  50.0, 0.0));

--        Link.Multicast (Network.Groups.Management_Channel,
--                        Network.Messages.Propose_Task (S (1)));
--        return;

      --  Send these jobs:
      for I in S'First .. Split loop
         Link.Multicast (Network.Groups.Management_Channel,
                         Network.Messages.Propose_Task (S (I)));
         Log ("Sent task " & S (I).To_String, Debug);

--           Debug2.Wait_For_Keypress;
--           delay 2.0;
      end loop;

      delay 2.0;

      for I in Split + 1 .. S'Last loop
         Link.Multicast (Network.Groups.Management_Channel,
                         Network.Messages.Propose_Task (S (I)));
         Log ("Sent task " & S (I).To_String, Debug);
      end loop;
   end;

   Link.Shutdown;

end Sancta.Main.Ficticio1b;
