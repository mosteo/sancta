with Sancta.Component.Root;

with Agpl.Protected_Datastore;
with Agpl.Xml;

with Ada.Calendar;

--  Player component.
--  Provides the gps_0 interface from player

--  This component accepts a "base_pose" attribute, which is where
--  we place the Origin for the meters reported by the GPS.
--  This is necessary since the large values given in meters overflow somewhere else.

package Sancta.Component.Gps is

   pragma Elaborate_Body;

   Plugin_Name : constant String := "gps";

   Requires_Odom_Pose : constant Internal_Key := "odom_pose";
   --  We use this odometry to give accurate pose since the last GPS update.

   Provides_Gps_Pose  : constant Internal_Key := "gps_pose";
   --  This is the pose directly obtained from the GPS.

   Provides_Mix_Pose  : constant Internal_Key := "mix_pose";
   --  This is the mixed GPS+odom pose that can be used, for example,
   --  as current pose.

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with
   record
      Base_Pose,
      Gps_Pose,
      Odom_Pose,
      Drift_Start_Pose : Types.Pose := Types.Origin;

      Reset_Mbicp : Boolean := True;

      Gps_Seconds : Duration := 0.0;
      --  Last time reported by the gps

      --  Convenience fields:
      As_Odom_Pose,
      As_Gps_Pose,
      As_Mix_Pose : access String;
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Agpl.Protected_Datastore.Object_Key;
                         Value : in     Agpl.Protected_Datastore.Object_Data'Class);

end Sancta.Component.Gps;
