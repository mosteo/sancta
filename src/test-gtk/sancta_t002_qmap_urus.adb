with Ada.Command_Line; use Ada.Command_Line;

with Agpl.Chronos;
with Agpl.Command_Line;
with Agpl.Drawing.Buffer;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Png;
with Agpl.Trace; use Agpl.Trace;

with Sancta.Map.Qtree;
with Sancta.Map.Qtree.Builder;
with Sancta.Map.Qtree.Show;
with Sancta.Map.Utils;
with Sancta.Types; use Sancta.Types;

--  This test only makes sense with the URUS map

procedure Sancta_T002_Qmap_Urus is
   M : Sancta.Map.Qtree.Object (Sancta.Map.Qtree.Vicinity_6);
   P : Agpl.Png.Png_File;

   Load_Timer : Agpl.Chronos.Object;
   Draw       : Agpl.Drawing.Buffer.Object;

   type Place is record
      Name : access String;
      X, Y : Float;
   end record;

   List : constant array (Positive range <>) of Place := (
      (new String'("Entrance A6"), 68.0, 38.0),
      (new String'("Right of A6"), 82.0, 9.0),
      (new String'("Left of A6 "), 55.0, 9.0),
      (new String'("Entrance A5"), 24.0, 38.0),
      (new String'("Right of A5"), 38.0, 9.0),
      (new String'("Entrance of B5 "), 16.0, 65.0),
      (new String'("Cafeteria"), 56.0, 58.0),
      (new String'("FIB Square"), 42.0, 52.0),
      (new String'("CS Faculty"), 62.0, 65.0),
      (new String'("Waiting Area 1"), 68.0, 9.0),
      (new String'("Waiting Area 2"), 46.0, 38.0),
      (new String'("Waiting Area 3"), 93.0, 38.0),
      (new String'("Waiting Area 1d"), 68.0, 7.5),
      (new String'("Waiting Area 1t"), 68.0, 11.0),
      (new String'("Right of A6d"), 82.0, 7.5),
      (new String'("Right of A6t"), 82.0, 11.0),
      (new String'("Left of A6d "), 55.0, 7.5),
      (new String'("Left of A6t "), 55.0, 11.0),
      (new String'("Right of A5d"), 38.0, 7.5),
      (new String'("Right of A5t"), 38.0, 11.0),
      (new String'("Left of A5"), 18.0, 9.0),
      (new String'("Middle of A5"), 28.0, 9.0),
      (new String'("Zero Zone"), 0.0, 0.0) );
begin
   if Argument_Count /= 1 then
      Log ("program <file.png>", Error);
      return;
   end if;

   Set_Level (Debug);
   Enable_Section (Agpl.Png.Log_Section);
   --   Enable_Section (Sancta.Map.Qtree.Det_Section);
   Enable_Section (Sancta.Map.Qtree.Log_Section);

   P.Open (Argument (1));

--     M.Set_Size      (-0.1, 93.0, -0.1, 95.0);

   M.Set_Cell_Size (0.5, 3.0);

   Sancta.Map.Qtree.Builder.From_Png (M, Argument (1),
                                     -0.1, 93.0, -0.1, 95.0);
   Log ("Loading time: " & Load_Timer.Image, Always);

   --  Random path in map to test path-finding
   Draw.Clear;
   Sancta.Map.Qtree.Show.Draw (M, D => Draw);
   for I in List'Range loop
      for J in List'Range loop
         if I < J then
            begin
               declare
                  Ini : constant Sancta.Map.Location'Class :=
                    M.Nearest_Location ((+List (I).X, +List (I).Y, 0.0));
                  Fin : constant Sancta.Map.Location'Class :=
                    M.Nearest_Location ((+List (j).X, +List (j).Y, 0.0));

                  Way_Timer : Agpl.Chronos.Object;
                  Way : constant Sancta.Map.Path_With_Cost := M.Best_Path (Ini, Fin);
               begin
                  --  Log ("Path: " & Sancta.Map.Image (Way.Path), Always);
                  Log ("Path-finding time: " & Way_Timer.Image, Always);

                  Sancta.Map.Utils.Draw_Path (Way.Path, Draw);
                  Sancta.Map.Utils.Draw_Pose
                    ((+List (I).X, +List (I).Y, 0.0), Draw);
                  Sancta.Map.Utils.Draw_Pose
                    ((+List (J).X, +List (J).Y, 0.0), Draw);
--                    Agpl.Command_Line.Wait_For_Keystroke;

                  --     Log ("Printing", Always);
                  --     M.Print;
               end;
            exception
               when E: Constraint_Error =>
                  --  No locations, show it without path
                  Log (Report (E), Error);
                  Log ("Unreachable: " & I'Img & J'Img, Always);
            end;
         end if;
      end loop;
   end loop;

   Agpl.Gdk.Managed.Drawing_Area.Show (Draw);
   Log ("The end", Always);

   delay 1_000_000.0; -- Avoid losing M scope.
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end Sancta_T002_Qmap_Urus;
