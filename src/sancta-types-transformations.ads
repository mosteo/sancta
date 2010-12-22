with Sancta.Gap;
with Agpl.Transf2D;

--  For frame changes

package Sancta.Types.Transformations is

   pragma Preelaborate;

   function Polar_To_Cart (A : in Types.Angle;
                           D : in Types.Real) return Types.Pose;
   pragma Inline (Polar_To_Cart);
   --  Pose.Angle will always be 0.0

   package Real_Transf is new Agpl.Transf2D (Types.Real);

   function "+" (P : in Types.Pose) return Real_Transf.Pose; pragma Inline ("+");
   function "+" (P : in Real_Transf.Pose) return Types.Pose; pragma Inline ("+");

   function Robot_To_World (Robot_Pose, Pose : in Types.Pose) return Types.Pose;
   --  Converts from Robot (not Odometry) coordinates to world coordinates.
   --  Robot_Pose is the pose of the robot in the world.
   --  Pose is the pose to convert.

   function World_To_Odom (Odometry_Pose : in Pose;
                           World_Pose    : in Pose;
                           Goal_Pose     : in Pose) return Pose;
   --  Given the odometry pose of the controller,
   --  the world robot pose and a goal in that world,
   --  obtain the Goal Pose in the Odometry frame.

   function Change_Frame (Frame1 : in Pose;
                          Frame2 : in Pose;
                          Pose2  : in Pose) return Pose renames World_To_Odom;
   --  Return Pose in Frame2 as seen from Frame1

   function To_World (This : in Sancta.Gap.Object;
                      RiW  : in Pose -- "Robot in World" pose.
                     ) return Sancta.Gap.Object;
   --  Convert a Gap in robot coordinates to world coordinates.

   procedure To_World (This : in out Sancta.Gap.Object_Array;
                       Riw  : in     Pose);

end Sancta.Types.Transformations;
