with Sancta.Ctree.Connectivity_Matrix;
with Sancta.Ctree.Robot;

with Sancta.Assignment;
with Sancta.Containers;
with Sancta.Cost_Matrix;
with Sancta.Map.Smart;
with Sancta.Types;
use Sancta,
    Sancta.Containers;

with Player.Client;
with Player.Graphics2d;

package Sancta.Ctree.Context is

   type Object is tagged limited record
      Pending_Tasks : Tc.Lists.List;
      Plan          : Tc.Lists.List;
      Team          : Assignment.Object;
      Costs         : Cost_Matrix.Object;

      Base      : String (1 .. 2) := " 1";
      Base_Pose : Sancta.Types.Pose;
      Clusters  : Connectivity_matrix.Object; -- Current, actual clusters
      The_Map   : Map.Smart.Object;

      Conn          : Player.Client.Connection_Type;
      Draw          : Player.Graphics2d.Object;
      Draw_Navigate : Tc.Lists.List; -- This is still cruft.
                                     --  Tasks to be drawn of navigate type.
   end record;

   function Base_Bot (This : Object) return Robot.Object;

end Sancta.Ctree.Context;
