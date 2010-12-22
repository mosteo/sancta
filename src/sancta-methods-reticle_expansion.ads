with Sancta.Types;

with Sancta.Method;
with Sancta.Plan_Node;
with Sancta.Tasks;

package Sancta.Methods.Reticle_Expansion is

   pragma Preelaborate;

   type Kinds is (Rect, Hex);

   type Object (Kind : Kinds) is new Sancta.Method.Object with record
      Rang : Types.Real := 3.0;
   end record;

   function Apply (This : in Object; That : in Sancta.Tasks.Object'Class)
                   return Sancta.Plan_Node.Node_Access;
   --  Applies to Search_Blob_In
   --  Generates Panorama_At_Coords
   --  The reticle expansion provides two exploration grids for any area:
   --  One made of squares and another made of hexagons.
   --  We'll see which is better.
   --  This will generate Panorama_At_Coords tasks, which should further be
   --  decomposed in Snapshot_At_Pose tasks.

end Sancta.Methods.Reticle_Expansion;
