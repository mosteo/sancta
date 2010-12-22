with Sancta.Component.Root,
     Sancta.Types;

with Ada.Calendar;

package Sancta.Component.Novatel is

   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.component.novatel";

   Name : aliased constant String := "novatel";

   Requires_Odom_Pose : constant Internal_Key := "odom_pose";
   --  We use this odometry to give accurate pose since the last GPS update.

   Provides_Gps_Pose  : constant Internal_Key := "gps_pose";
   --  This is the pose directly obtained from the GPS.

   Provides_Mix_Pose  : constant Internal_Key := "mix_pose";
   --  This is the mixed GPS+odom pose that can be used, for example,
   --  as current pose.

   Option_Base_Pose : constant Option_Attr := "option_base_pose";
   Option_Port      : constant Option_Attr := "port";
   --  The serial port to get readings (e.g. /dev/ttyUSB0)

   procedure Register;

private

   type Object is new Root.Object with
   record
      Base_Pose,
      Gps_Pose,
      Odom_Pose,
      Drift_Start_Pose : Types.Pose := Types.Origin;

      Gps_Seconds : Duration := 0.0;
      --  Last time reported by the gps

      --  Convenience fields:
      As_Odom_Pose,
      As_Gps_Pose,
      As_Mix_Pose : access String;
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Novatel;
