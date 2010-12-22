with Sancta.Map;
with Sancta.Map.Bitmap;
with Sancta.Map.Bitmap_Wavefront_8;
with Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;
with Sancta.Types;
use Sancta; use Sancta.Map; use Sancta.Map.Bitmap; use Sancta.Types;

--  with Agpl.Text_Io; use Agpl.Text_Io;

procedure T003_Alternate is

   package Navigate renames Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;

   M : Map.Bitmap.Object (Map.Bitmap.Vicinity_4);

   O : Map.Bitmap.Terrains renames Map.Bitmap.Free;
   X : Map.Bitmap.Terrains renames Map.Bitmap.Obstacle;

   B : constant Map.Bitmap.Matrix (1 .. 5, 1 .. 5) :=
         (1 => (X, X, X, X, X),
          2 => (X, O, O, O, X),
          3 => (X, O, X, O, X),
          4 => (X, O, O, O, X),
          5 => (X, X, X, X, X));

   Ini : constant Types.Pose := (2.5, 2.5, 0.0);
   Fin : constant Types.Pose := (4.5, 4.5, 0.0);

begin
   M.Set (B);
   M.Set_Cell_Size (1.0);

   declare
      R : constant Map.Path :=
            Sancta.Map.Bitmap_Wavefront_8
              (M.To_Grid (Ini),
               M.To_Grid (Fin),
               M);
      A : constant Map.Path :=
            Navigate.Get_Alternate_Path
              (M,
               Ini,
               Fin,
               R);

   begin
      M.Print (R);
      M.Print (A);
   end;
end t003_Alternate;
