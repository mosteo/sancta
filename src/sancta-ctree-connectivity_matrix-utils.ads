with Sancta.Ctree.Team_Tree,
     Sancta.Assignment;

use  Sancta;

package Sancta.Ctree.Connectivity_Matrix.Utils is

   --  pragma Preelaborate;

   procedure Set_Umbral (U : Link_Qualities);
   --  used in From_Datastore

   function From_Datastore (Num_Bots : Natural) return Object;

   procedure Merge_Keep_Weak (This : in out Object; Add : Object);
   --  Merges two matrices, in such a way that any link that is already
   --  weak in This won't be changed to strong regardless of Add

   procedure Make_All_Weak_For_Idle_Clusters (This : in out Object;
                                              Ass  :        Assignment.Object);
   --  Robots belonging to an idle group will have its links promoted to weak.

   function Keep_Only_Tree (This : Object;
                            Tree : Ctree.Team_Tree.Object'Class)
                            return Object;
   --  Keep only links in a given tree

end Sancta.Ctree.Connectivity_Matrix.Utils;
