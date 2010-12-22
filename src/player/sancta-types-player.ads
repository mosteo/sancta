with Player;

package Sancta.Types.Player is

   pragma Preelaborate;

   package P renames Standard.Player;

   function To_Pose (P : in Standard.Player.Pose) return Pose;
   --  Convert Player poses to Sancta poses.

   function To_Pose (P : in Types.Pose) return Standard.Player.Pose;
   --  Sancta to Player pose.

   function "+" (X : in Types.Real) return P.Double;

   function "+" (X : in P.Double) return Types.Real;

   function "+" (X : in P.Double) return Float;

   function "+" (X : in Float) return P.Double;

   function "+" (X : in P.Pose) return Types.Pose;

   function "+" (X : in Types.Pose) return P.Pose;

   function "+" (X : in Types.Angle) return Standard.Player.Double;

   function "+" (X : in Types.Pose) return Standard.Player.Point_2d;

private

   pragma Inline ("+");

end Sancta.Types.Player;
