with Sancta.Ctree.Connectivity_Matrix;

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Cost_Cache;
with Sancta.Tasks.Containers;
with Sancta.Types;

package Sancta.Ctree.Strategies is

   --  pragma Preelaborate;

   package Ac renames Sancta.Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;

   type Object is abstract tagged private;

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class) --  S-Clusters
   is abstract;

   procedure Set_Base_Pose (This : in out Object;
                            Pose :        Sancta.Types.Pose);

   function Get_Base_Pose (This : Object) return Sancta.Types.Pose;


private

   type Object is abstract tagged record
      Base_Pose : Sancta.Types.Pose;
   end record;

end Sancta.Ctree.Strategies;
