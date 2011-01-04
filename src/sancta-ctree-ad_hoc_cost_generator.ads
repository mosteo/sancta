with Sancta.Cost_Utils.Generator_Positioned.Euclidean,
     Sancta.Tasks,
     Sancta.Types;

use Sancta;

package Sancta.Ctree.Ad_Hoc_Cost_Generator is

   Log_Section : constant String := "nerus.ad_hoc_cost_generator";

   type Object is
     new Cost_Utils.Generator_Positioned.Euclidean.Object with private;

   overriding
   function Create (At_Pose : Types.Pose) return Object;

   overriding
   function Get_Cost (This : Object;
                      Ini  : Sancta.Tasks.Object'Class;
                      Fin  : Sancta.Tasks.Object'Class)
                      return Sancta.Costs;

private

   type Object is
     new Cost_Utils.Generator_Positioned.Euclidean.Object with null record;

end Sancta.Ctree.Ad_Hoc_Cost_Generator;
