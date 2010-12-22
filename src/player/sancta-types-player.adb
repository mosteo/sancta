package body Sancta.Types.Player is

   -------------
   -- To_Pose --
   -------------

   function To_Pose (P : in Standard.Player.Pose) return Pose is
   begin
      return
        (X => Real (P (1)),
         Y => Real (P (2)),
         A => Angle (P (3)));
   end To_Pose;

   -------------
   -- To_Pose --
   -------------

   function To_Pose (P : in Types.Pose) return Standard.Player.Pose is
   begin
      return (Standard.Player.Double (P.X),
              Standard.Player.Double (P.Y),
              Standard.Player.Double (P.A));
   end To_Pose;

   ---------
   -- "+" --
   ---------

   function "+" (X : in Types.Real) return P.Double is
   begin
      return P.Double (X);
   end "+";

   function "+" (X : in Types.Angle) return P.Double is
   begin
      return P.Double (X);
   end "+";

   function "+" (X : in P.Double) return Types.Real is
   begin
      return Types.Real (X);
   end "+";

   function "+" (X : in Float) return P.Double is
   begin
      return P.Double (X);
   end "+";

   function "+" (X : in P.Pose) return Types.Pose is
   begin
      return (X => + X (1),
              Y => + X (2),
              A => Types.Angle (X (3)) + 0.0);
   end "+";

   function "+" (X : in P.Double) return Float is
   begin
      return Float (X);
   end "+";

   function "+" (X : in Types.Pose) return P.Pose is
   begin
      return (+ X.X, + X.Y, P.Double (X.A));
   end "+";

   function "+" (X : in Types.Pose) return P.Point_2d is
      use P;
   begin
      return (P.Double (X.X), P.Double (X.Y));
   end "+";

end Sancta.Types.Player;
