with Sancta.Types;

with Sancta.Plan_Node;
with Sancta.Method;
with Sancta.Tasks;

package Sancta.Methods.Explore_Segment_Expansion is

   pragma Preelaborate;

   use type Types.Real;

   type Object is new Sancta.Method.Object with null record;

   subtype Result is Sancta.Plan_Node.Node_Access;

   function Apply (This : in Object; That : in Sancta.Tasks.Object'Class)
                   return Result;
   --  Applies to Explore_Segment tasks.
   --  Generates two ORed Explore_Directed_Segment

   --  Also works on Explore_Edge tasks

   Instance : constant Object := (Sancta.Method.Object with null record);

end Sancta.Methods.Explore_Segment_Expansion;
