with Sancta.Ctree.Tree_Navigator.Bitmap,
     Sancta.Types;

package Sancta.Ctree.Tree_Navigator.Bitmap_Ordered is

   type Object is new Bitmap.Object with private;

   Log_Section : constant String := "Sancta.Ctree.tree_navigator.bitmap_ordered";

   not overriding
   function Order (This : Object) return Tc.Lists.List;
   --  Get tasks in the order they are to be executed

   not overriding
   procedure Set_Order (This : in out Object; Jobs : Tc.Lists.List)
                        renames Set_Tasks;

   type Creators is (Mst, Ctsp, Steiner);
   --  MST: minimum spanning tree
   --  CTSP: closed TSP

   type Creator_Function is access
     function
       (Base : Types.Pose;
        Head : Types.Pose;
        Jobs : Tc.Lists.List;
        M    : Map.Object'Class;
        Lim  : Costs) return Object'Class;

   function Create_With_Mst
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class;
   --  Defaults to depth-first order

   function Create_With_Ctsp
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class;
   --  Assumes Jobs is already in CTSP order

   function Create_With_Steiner
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class;
   --  Defaults to depth-first order

   function Available_Creators (C : Creators) return Creator_Function;
   --  Needed because of a bug that prevents using an array.

   function Steiner_Heuristic_Closest
     (This : Object'Class;
      M    : Map.Object'Class) return Object;
   --  Get some solution and apply the "Closest" transform.
   --  The closest transform modifies a solution this way (example MST):
   --    We move to the next task, not following the proposed route, which may
   --    be either from the previous task or some other previous task in the MST,
   --    but from the closest point in the previous task branch to the next task.

private

   type Object is new Bitmap.Object with null record;

   function Prefix (M        : Map.Object'Class;
                    Ini, Fin : Types.Pose)
                    return     Map.Path;
   --  Return prefix route from Ini to Fin

end Sancta.Ctree.Tree_Navigator.Bitmap_Ordered;
