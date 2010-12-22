with Sancta.Component.Root;

with Agpl.Xml;

with Ada.Calendar;

package Sancta.Component.Laser is

   pragma Elaborate_Body;

   Plugin_Name            : constant String := "laser";

   Log_Section            : constant String := "sancta.Component.laser";

   Requires_Loc_Pose      : constant Internal_Key := "loc_pose";

   Provides_Scan          : constant Internal_Key := "scan";
   Provides_Scan_Id       : constant Internal_Key := "scan_id";
   Provides_Pose_On_Robot : constant Internal_Key := "pose_on_robot";
   --  The pose of the laser respect to the robot.
   --  Necessary for laser reading corrections...

   type Object is new Root.Object with private;
   --  We implement different robotic capabilities as plugins who manipulate
   --  a common protected datastore. So each component can run synchronously
   --  or have its own tasks.

   function Create (Config : in Agpl.Xml.Node) return Object_Access;
   --  For a factory approach

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      Scan_Id : Integer := 0;

      --  Convenience fields:
      As_Loc_Pose,

      As_Scan,
      As_Scan_Id,
      As_Pose_On_Robot : access String;
   end record;

end Sancta.Component.Laser;
