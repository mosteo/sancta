with Agpl.Containers.Unbounded_Trees;

with Sancta.Map;

package Sancta.Ctree.Path_Trees is

   type Node_Data is tagged null record;

   package Trees is
     new Agpl.Containers.Unbounded_Trees
       (Node_Data'Class,
        Sancta.Map.Relative_Position'Class,
        Sancta.Map.Class_Less_Than);

   subtype Tree   is Trees.Tree;
   subtype Cursor is Trees.Cursor;

end Sancta.Ctree.Path_Trees;
