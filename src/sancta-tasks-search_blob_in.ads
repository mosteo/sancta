with Sancta.Types;

with Sancta.Tasks.Compound;
with Agpl; use Agpl;

package Sancta.Tasks.Search_Blob_In is

   pragma Preelaborate;

   type Shapes is (Rectangle);

   type Object (Shape : Shapes) is new
     Sancta.Tasks.Compound.Object with
      record
         Color     : Types.Colors;
         Tolerance : Types.Percent := 0.0; -- Exact match.
         case Shape is
            when Rectangle =>
               Left,
               Top,
               Right,
               Bottom : Types.Real := 0.0;
         end case;
      end record;
   --  Values in meters.

   function Create_Rectangle
     (Left,
      Top,
      Right,
      Bottom    : in Types.Real;
      Color     : in Types.Colors;
      Tolerance : in Types.Percent := 0.0) return Object;

end Sancta.Tasks.Search_Blob_In;
