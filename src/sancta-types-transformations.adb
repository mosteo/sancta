--  with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Types.Real_Math;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Types.Transformations is

   use Real_Math;
   use Real_Transf;

   -------------------
   -- Polar_To_Cart --
   -------------------

   function Polar_To_Cart (A : in Types.Angle;
                           D : in Types.Real) return Types.Pose
   is
   begin
      return (D * Cos (Real (A)),
              D * Sin (Real (A)),
              0.0);
   end Polar_To_Cart;

   ---------
   -- "+" --
   ---------

   function "+" (P : in Types.Pose) return Real_Transf.Pose is
   begin
      return (P.X, P.Y, Real (P.A), 1.0);
   end "+";

   function "+" (P : in Real_Transf.Pose) return Types.Pose is
      Q : constant Real_Transf.Pose := Homogeneize (P);
   begin
      return (Q (1), Q (2), Angle (Q (3)));
   end "+";

   --------------------
   -- Robot_To_World --
   --------------------

   function Robot_To_World (Robot_Pose, Pose : in Types.Pose) return Types.Pose is
   begin
      return +Compose (+Robot_Pose, +Pose);
   end Robot_To_World;

   -------------------
   -- World_To_Odom --
   -------------------

   function World_To_Odom (Odometry_Pose : in Pose;
                           World_Pose    : in Pose;
                           Goal_Pose     : in Pose) return Pose
   is
   begin
      return +Compose (+Odometry_Pose,
                       Decompose (+World_Pose, +Goal_Pose));
   end World_To_Odom;

   --------------
   -- To_World --
   --------------

   function To_World (This : in Sancta.Gap.Object;
                      RiW  : in Pose -- "Robot in World" pose.
                     ) return Sancta.Gap.Object
   is
      use Agpl;
      use Sancta.Gap;
      P1 : constant Cv.Point2D := Get_Start (This);
      P2 : constant Cv.Point2D := Get_End (This);
      X1 : constant Pose := ( + P1 (1), + P1 (2), 0.0);
      X2 : constant Pose := ( + P2 (1), + P2 (2), 0.0);
      N1 : constant Pose := Robot_To_World (RiW, X1);
      N2 : constant Pose := Robot_To_World (RiW, X2);
      Z1 : constant Cv.Point2D := ( + N1.X, + N1.Y, 1.0);
      Z2 : constant Cv.Point2D := ( + N2.X, + N2.Y, 1.0);
   begin
      return Create (Z1, Z2, Get_Kind (This));
   end To_World;
   --  Convert a Gap in robot coordinates to world coordinates.

   --------------
   -- To_World --
   --------------

   procedure To_World (This : in out Sancta.Gap.Object_Array;
                       Riw  : in     Pose) is
   begin
      for I in This'Range loop
         This (I) := To_World (This (I), RiW);
      end loop;
   end To_World;

end Sancta.Types.Transformations;
