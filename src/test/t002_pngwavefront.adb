with Sancta.Map;
with Sancta.Map.Bitmap;
with Sancta.Map.Wavefront;
use Sancta;

with Agpl.Text_Io; use Agpl.Text_Io;

with Ada.Command_Line; use Ada.Command_Line;

procedure t002_pngwavefront is

   M : aliased Map.Bitmap.Object (Map.Bitmap.Vicinity_4);

begin
   M.From_Png (Argument (1),
               Sancta.Map.Bitmap.Stage_Loader);

   declare
      function Wave is new Map.Wavefront (Map.Bitmap.Vicinity_4,
                                          Map.Bitmap.Get_Cost);

      R : constant Map.Location_Lists.List :=
            Wave (Map.Bitmap.Bit_Location'(M'Unchecked_Access, X => 1,  Y => 0),
                  Map.Bitmap.Bit_Location'(M'Unchecked_Access, X => 14, Y => 8),
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
      M.Print;
      M.Print (R);
   end;
end t002_pngwavefront;
