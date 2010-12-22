with Sancta.Component.Root;

with Agpl.Xml;

with Player.Position2d;

with Ada.Calendar;

--  Player component.
--  Provides the Position2d_0 interface from player
--  Sends commands also to that interface.
--  Because of that, the goal pose should be in the same reference that the
--  player interface is using.

--  Example: if position2d:0 is VFH reading from position2d:1 (odometry),
--  we should give the goal pose in the position2d:1 reference
--  (which in this case is the same as position2d:0).

package Sancta.Component.Odometry is

   pragma Elaborate_Body;

   Plugin_Name : constant String := "odometry";

   Log_Section : constant String := "sancta.component.odometry";

   --  Data key for the config file:
   Provides_Pose     : constant Internal_Key := "odom_pose";
   Provides_Pose2    : constant Internal_Key := "odom_pose2";
   Provides_Velocity : constant Internal_Key := "velocity";

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      --  Convenience fields:
      As_Pose,
      As_Pose2,
      As_Velocity : access String;

      Pos : Player.Position2d.Object;
   end record;

end Sancta.Component.Odometry;
