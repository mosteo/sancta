 

with Sancta.Assignment;

package Sancta.Cost_Matrix.Utils is

   pragma Preelaborate;

   procedure Create (Dst : in out Cost_Matrix.Object;
                     Ass : in     Assignment.Object);
   --  Will create all needed costs to evaluate Ass

   procedure Prune (Dst :    out Cost_Matrix.Object;
                    Src : in     Cost_Matrix.Object;
                    Ass : in     Assignment.Object);
   --  Copy the used costs in Ass from Src to Dst, discarding the rest.

end Sancta.Cost_Matrix.Utils;
