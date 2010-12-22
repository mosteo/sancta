with Sancta.Config;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Tasks.Explore_Segment;
with Sancta.Types; use Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;

with Ada.Text_Io; use Ada.Text_Io;

procedure Sancta.Main.Ada_Con_Coches1 is

   --  There are three segments to explore to visit all cars.
   --  We'll propose these three compound tasks.
   --  These must be received by the planner (initially the TSP one).
   --  All plans should be computed, the best one chosen, and the tasks
   --  propagated to the robots.

   Our_Id : aliased Node_Id;
   --  We won't listen to messages, it is used simply to be able to transmit.

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);

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
      S1 : constant Tasks.Explore_Segment.Object :=
             Tasks.Explore_Segment.Create ((-41.0, 17.0, 0.0), (34.0, 17.0, 0.0));
      S2 : constant Tasks.Explore_Segment.Object :=
             Tasks.Explore_Segment.Create ((-41.0, 0.0, 0.0), (34.0, 0.0, 0.0));
      S3 : constant Tasks.Explore_Segment.Object :=
             Tasks.Explore_Segment.Create ((-41.0, -17.0, 0.0), (34.0, -17.0, 0.0));
      S4 : constant Tasks.Explore_Segment.Object :=
             Tasks.Explore_Segment.Create ((34.5, 25.0, 0.0), (34.5, -17.0, 0.0));
   begin
      --  Send these jobs:
      Link.Multicast (Network.Groups.Management_Channel,
                      Network.Messages.Propose_Task (S1));
      Log ("Sent task " & S1.To_String, Debug);
      Link.Multicast (Network.Groups.Management_Channel,
                      Network.Messages.Propose_Task (S2));
      Log ("Sent task " & S2.To_String, Debug);
      Link.Multicast (Network.Groups.Management_Channel,
                      Network.Messages.Propose_Task (S3));
      Log ("Sent task " & S3.To_String, Debug);
      Link.Multicast (Network.Groups.Management_Channel,
                      Network.Messages.Propose_Task (S4));
      Log ("Sent task " & S4.To_String, Debug);
   end;

   Link.Shutdown;

end Sancta.Main.Ada_con_coches1;
