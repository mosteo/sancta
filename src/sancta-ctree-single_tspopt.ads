with Sancta.Ctree.Robot,
     Sancta.Containers,
     Sancta.Cost_Matrix,
     Sancta.Types;

use Sancta,
    Sancta.Containers;

package Sancta.Ctree.Single_Tspopt is

   function Build_Plan (Bot    : Robot.Object;
                        Jobs   : Tc.Lists.List;
                        Costs  : Cost_Matrix.Object'Class;
                        Init   : Boolean := True) return Tc.Lists.List;
   --  Build a plan for a posed robot.
   --  If Init will use a copy with all costs, computing starting ones.
   --  Beware that this may be expensive...

   function Build_Plan (From   : Types.Pose;
                        Jobs   : Tc.Lists.List;
                        Costs  : Cost_Matrix.Object'Class) return Tc.Lists.List;

   --  In the CTree context, this bot represents the backtracking point for
   --  the start of a new, optimal plan (the base or surrogate aprox point).

end Sancta.Ctree.Single_Tspopt;
