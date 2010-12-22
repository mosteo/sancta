 

with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;
use Agpl;

function Sancta.Plan_Node.Get_Random_Sibling (This : in Node_Access)
                                                return    Node_Access
is
   use Node_Vectors;
begin
   --  If this is not an OR node...
   if This.Parent = null or else Get_Kind (This.Parent) /= Or_Node then
      Log ("Plan_Node.Get_Random_Sibling: Parent node is " &
           Node_Kind'Image (Get_Kind (This.Parent)),
           Warning);
      return This;
   end if;

   return Element
     (This.Parent.Children,
      Random.Get_Integer (First_Index (This.Parent.Children),
        Last_Index  (This.Parent.Children)));
end Sancta.Plan_Node.Get_Random_Sibling;

