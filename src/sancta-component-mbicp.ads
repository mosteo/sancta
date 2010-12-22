with Sancta.Component.Ctypes,
     Sancta.Component.Root,
     Sancta.Types;

private with System;

--  Player component.
--  Provides the gps_0 interface from player

--  This component accepts a "base_pose" attribute, which is where
--  we place the Origin for the meters reported by the GPS.
--  This is necessary since the large values given in meters overflow somewhere else.

package Sancta.Component.Mbicp is

   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.Component.mbicp";

   Name : aliased constant String := "mbicp";

   Requires_Scan          : constant Internal_Key := "scan";
   Requires_Odom_Pose     : constant Internal_Key := "odom_pose";
   Requires_Laser_Pose_On_Robot : constant Internal_Key := "laser_pose_on_robot";

   Provides_Improved_Pose : constant Internal_Key := "improved_pose";
   Provides_Improved_Scan : constant Internal_Key := "improved_scan";

   Option_Out_Of_Range : constant Option_Attr := "out_of_range";

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   not overriding
   procedure Reset (This : in out Object);
   --  Will cause resetting of pose to the odometry one!

   procedure Register;

private

   task type Calculator_Type (Parent : access Object) is
      pragma Priority (System.Max_Priority);

      entry New_Scan (Scan : in Ctypes.Range_Scan;
                      Pose : in Sancta.Types.Pose);
   end Calculator_Type;
   type Calculator_Access is access Calculator_Type;

   type Object is new Root.Object with
   record
      Odom_Pose         : Sancta.Types.Protected_Pose.Object;
      Odom_Base_Pose    : Types.Pose;
      Mbicp_Pose        : Sancta.Types.Protected_Pose.Object;
      Mbicp_Pose_Inited : Boolean := False;

      Calculator        : Calculator_Access;
   end record;

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Mbicp;
