with Sancta.Component.Root;
with Sancta.Component.Types;
with Sancta.Types;

with Agpl.Xml;

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Text_Io;

--  Logs laser in montesano's SM format

package Sancta.Component.Logger_Montesano is

   pragma Elaborate_Body;

   Name : aliased constant String := "logger_montesano";

   Requires_Scan : constant Internal_Key := "scan";
   Requires_Pose : constant Internal_Key := "pose";

   Option_Filename       : constant Option_Attr := "filename";
   Option_Immediate      : constant Option_Attr := "immediate";
   Option_Use_Laser_Pose : constant Option_Attr := "use_laser_pose";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   overriding
   procedure Stop (This : in out Object);

   procedure Register;

private

   --  The non-indefinite vectors will cause a bug box here if used!?
   package Dist_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive,
      Sancta.Types.Real,
      Sancta.Types."=");

   type Reading is record
      Odom : Sancta.Types.Pose;
      Dist : Dist_Vectors.Vector;
   end record;

   package Reading_Vectors is new Ada.Containers.Vectors (Positive, Reading);

   type Object is new Root.Object with
      record
         Data      : Reading_Vectors.Vector;

         Prev_Pose : Sancta.Types.Pose := Sancta.Types.Origin;

         Filename  : access String;
         File      : Ada.Text_Io.File_Type;
         Immediate     : Boolean;
         First_Written : Boolean := False;
         Use_Laser_Pose: Boolean := True;

         --  Convenience fields:
         As_Scan,
         As_Pose : access String;
      end record;

   procedure Add_Scan (This : in out Object;
                       Scan : in     Types.Range_Scan);

   procedure Write_Reading (This : in Reading;
                            File : in out Ada.Text_Io.File_Type);

   procedure Write_Scans (This : in Object);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Logger_Montesano;
