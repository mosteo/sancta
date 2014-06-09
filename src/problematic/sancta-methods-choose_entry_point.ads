with Sancta.Types;

with Sancta.Plan_Node;
with Sancta.Method;
with Sancta.Tasks;

package Sancta.Methods.Choose_Entry_Point is

   pragma Preelaborate;

   use type Types.Real;

   type Object is new Sancta.Method.Object with null record;

   subtype Result is Sancta.Plan_Node.Node_Access;

   function Apply (This : in Object;
                   That : in Sancta.Tasks.Object'Class)
                   return    Result;
   --  Applies to Choose_Entry_Point tasks.
   --  Generates ORed Entry_Point

   Instance : constant Object := (Sancta.Method.Object with null record);

end Sancta.Methods.Choose_Entry_Point;
