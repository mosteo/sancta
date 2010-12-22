with Sancta.Component.Root,
     Sancta.Types;

with Agpl.Chronos;
with Agpl.Xml;

with Ada.Calendar;

--  Simulates the behavior of the Garmin GPS, for testing.

package Sancta.Component.Gps_Sim is

   pragma Elaborate_Body;

   Name : aliased constant String := "gps_sim";

   Requires_Odom_Pose : constant Internal_Key := "odom_pose";
   --  We use this odometry to give accurate pose since the last GPS update.

   Provides_Gps_Pose  : constant Internal_Key := "gps_pose";
   --  This is the pose directly obtained from the GPS.

   Provides_Mix_Pose  : constant Internal_Key := "mix_pose";
   --  This is the mixed GPS+odom pose that can be used, for example,
   --  as current pose.

   Option_Base_Pose : constant Option_Attr := "base_pose";
   --  Starting pose

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   use type Types.Real;

   type Object is new Root.Object with
   record
      Base_Pose,
      Gps_Pose,
      Odom_Pose,
      Drift_Start_Pose : Types.Pose := Types.Origin;

      Ini_Pose         : Types.Pose := (29.89, -16.28, 0.0);
      Fin_Pose         : Types.Pose := (-32.58, -16.0, 0.0);
      Start            : Agpl.Chronos.Object;

      Gps_Seconds      : Duration := 0.0;
      --  Last time reported by the gps

      --  Convenience fields:
      As_Odom_Pose,
      As_Gps_Pose,
      As_Mix_Pose : access String;
      end record;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     internal_Key;
                         Value : in     Data'Class);

   procedure Get_Simulated_Pose (This : in out Object;
                                 Pose :    out Types.Pose);

end Sancta.Component.Gps_Sim;
