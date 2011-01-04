with Sancta.Types;
use Sancta;

package Sancta.Ctree.Agent_Vector is

   type Object is tagged private;

   procedure Add (This : in out Object;
                  Pose :        Types.Pose);
   --  Add a vector to the mix.

   function Result (This : Object) return Types.Pose;
   --  Get the resulting vector.

   procedure Clear (This : in out Object);
   --  Start afresh

private

   type Object is tagged record
      V : Sancta.Types.Pose_Vector.Object (First => 1);
   end record;

end Sancta.Ctree.Agent_Vector;
