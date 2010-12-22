with Sancta.Types;

package Sancta.Convert is

   pragma Preelaborate;

   function To_Pose (S : in String) return Types.Pose;
   --  Accepts (x, y, a) and "x y z" formats.

end Sancta.Convert;
