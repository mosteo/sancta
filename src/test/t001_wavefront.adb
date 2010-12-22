with Sancta.Map;
with Sancta.Map.Bitmap;
with Sancta.Map.Wavefront;
use Sancta;

with Agpl.Text_Io; use Agpl.Text_Io;

procedure t001_wavefront is
   M : aliased Map.Bitmap.Object (Sancta.Map.Bitmap.Vicinity_4);

   O : Map.Bitmap.Terrains renames Map.Bitmap.Free;
   X : Map.Bitmap.Terrains renames Map.Bitmap.Obstacle;

   B : constant Map.Bitmap.Matrix (1 .. 4, 1 .. 4) :=
         (1 => (O, O, O, O),
          2 => (X, X, X, O),
          3 => (O, O, X, O),
          4 => (O, O, O, O));

begin
   M.Set (B);

   declare
      function Wave is new Map.Wavefront (Map.Bitmap.Vicinity_4,
                                          Map.Bitmap.Get_Cost);

      R : constant Map.Location_Lists.List :=
            Wave (Map.Bitmap.Bit_Location'(M'Unchecked_Access, X => 1, Y => 3),
                  Map.Bitmap.Bit_Location'(M'Unchecked_Access, X => 1, Y => 1),
                  M);

      use Map.Location_Lists;
      use Map.Bitmap;
      procedure Print (I : Cursor) is
         L : constant Bit_Location := Bit_Location (Element (I));
      begin
         Put_Line ("Loc:" & L.X'Img & L.Y'Img);
      end Print;
   begin
      R.Iterate (Print'Access);
   end;

   --  Test the computing of costs:
   M.Compute_Costs (Map.Bitmap.Vicinity_4'Access,
                    Map.Bitmap.Get_Cost'Access);
   Put_Line ("Cost:" & M.Get_Cost_Between (1, 3, 1, 1)'Img);
end t001_wavefront;
