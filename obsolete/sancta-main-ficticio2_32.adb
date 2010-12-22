with Sancta.Config;
--  with Sancta.Debug2;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Network.Messages;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Explore_Segment;
with Sancta.Types; use Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;

with Ada.Text_Io; use Ada.Text_Io;

procedure Sancta.Main.Ficticio2_32 is

   --  In this case, there are Choose_Entry_Point tasks so the
   --  requeteoptimal plan is computed

   --  There're 32 segments to explore (a lot! 4GB different plans!)

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

   declare
      E : array (1 .. 4) of Tasks.Choose_Entry_Point.Object :=
            (others =>
                   Tasks.Choose_Entry_Point.Create
               ((1 => (0.0, 58.0, -3.14159265),
                 2 => (0.0,-58.0, 3.1415926535))));
   begin
      --  Send these jobs:
      for I in E'Range loop
         Link.Multicast (Network.Groups.Management_Channel,
                         Network.Messages.Propose_Task (E (I)));
         Log ("Sent task " & E (I).To_String, Debug);
      end loop;
   end;

   --  Create the segments to explore:
   --  Angles are ignored to compute cost of these tasks.
   declare
      S  : array (1 .. 32) of Tasks.Explore_Segment.Object;
      X  : Types.Real := -50.0;
   begin
      for I in 1 .. 16 loop
         S (I) := Tasks.Explore_Segment.Create (( -6.0, X, 0.0),
                                                (-75.0, X, 0.0));
         S (I + 16) := Tasks.Explore_Segment.Create (( 6.0, X, 0.0),
                                                     (75.0, X, 0.0));
         X := X + (100.0 / Types.Real (S'Length / 2 - 1));
      end loop;

      --  Send these jobs:
      for I in S'Range loop
         Link.Multicast (Network.Groups.Management_Channel,
                         Network.Messages.Propose_Task (S (I)));
         Log ("Sent task " & S (I).To_String, Debug);
      end loop;

   end;

   Link.Shutdown;

exception
   when E : others =>
      Log ("Exception: " & Report (E), Error);
      Link.Shutdown;
end Sancta.Main.Ficticio2_32;
