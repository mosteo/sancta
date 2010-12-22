with Sancta.Tasks.Primitive;
with Sancta.Tasks.Containers;

package Sancta.Anneal is

--   pragma Elaborate_Body;

   --  Facilities for Annealing planner

   function Flip (This : in Sancta.Tasks.Primitive.Object'Class)
                  return Sancta.Tasks.Primitive.Object'Class;
   --  Given a task with OR parent, get its sibling

   function Get_All_Children (This : in Sancta.Tasks.Object'Class)
                              return Sancta.Tasks.Containers.Vectors.Vector;
   --  For an OR node, return all single leaf expansions
   --  For a primitive node, return itself.
   --  For non-OR or non-single-leaf expansions, raise Constraint_Error

   function Get_Any_Child (This : in Sancta.Tasks.Object'Class)
                           return    Sancta.Tasks.Primitive.Object'Class;
   --  if This is compound, expansion will be generated.
   --  If it's an OR generation of single leafs, return any of these.
   --  If not, raise Constraint_Error.
   --  if This was primitive in the first place, return it unchanged.

   function Parent (This : in Sancta.Tasks.Primitive.Object'Class)
                    return    Sancta.Tasks.Object'Class;
   --  Given a task with OR parent, get its parent

end Sancta.Anneal;
