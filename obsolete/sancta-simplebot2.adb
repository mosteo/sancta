--  Extends Simplebot adding pose beaconing. It requires a Sancta.Robot.Object
--   as agent.

with Sancta.Config;
with Sancta.Gui.Robot_Data;
with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Component.Mbicp;

with Agpl.Protected_Datastore;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Simplebot2 is

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      --  Parent:
      Simplebot.Object (This).Init;

      Register (This,
                Network.Messages.Set_Pose_Type'Tag,
                Cb_Set_Pose'Access);
   end Init;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean)
   is
   begin
      Simplebot.Object (This).Run (Done);

      --  Send a position update every split second:
      if This.Pose_Cron.Elapsed >= 0.5 then
         This.Pose_Cron.Reset;

         This.Link.Multicast (Network.Groups.Gui_Channel,
                              Gui.Robot_Data.Network_Update'
                                (Network.Message with
                                 Kind     => Gui.Robot_Data.Pose,
                                 Position => This.Bot.Get_Pose,
                                 Velocity => (0.0, 0.0, 0.0)));
         pragma Incomplete ("Velocity should be passed");

         begin
            This.Link.Multicast (Network.Groups.Gui_Channel,
                                 Network.Messages.Laser);
         exception
            when Agpl.Protected_Datastore.Data_Not_Present =>
               null;
         end;
      end if;
   end Run;

   -----------------
   -- Cb_Set_Pose --
   -----------------

   procedure Cb_Set_Pose (This : in out Netlistener.Object'Class;
                          M    : in     Network.Message'Class;
                          Meta : in     Network.Message_Metadata)
   is
      use Component;
      Pose : Network.Messages.Set_Pose_Type renames
        Network.Messages.Set_Pose_Type (M);
   begin
      Log ("Received pose correction from " & Network.Image (Meta.Sender),
           Debug, Section => Log_Section);

      --  Reset mbicp so it doesn't correct our manually altered pose.
      if Config.Get_Plugin (Component.Mbicp.Plugin_Name) /= null then
         Component.Mbicp.Object
           (Config.Get_Plugin (Component.Mbicp.Plugin_Name).all).Reset;
      end if;

      Simplebot2.Object (This).Bot.Set_Pose (Pose.Pose);
   end Cb_Set_Pose;

end Sancta.Simplebot2;
