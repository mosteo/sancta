with Agpl.Png;
with Agpl.Scaling2d;

package body Sancta.Map.Qtree.Builder is

   --------------
   -- From_Png --
   --------------

   procedure From_Png (This        : in out Object;
                       File        :        String;
                       Xmin, Xmax  :        X_Real;
                       Ymin, Ymax  :        Y_Real;
                       Buggy_Stage :        Boolean := True;
                       Id          :        String  := Default_Map_Id)
   is
      T : constant array (Agpl.Types.Unsigned_8) of Terrains :=
            (0 .. 127 => Obstacle, 128 .. 255 => Free);

      package Real_Scaling is new Agpl.Scaling2d (Types.Real);
      use Agpl.Png;
      F     : Png_File;
      Scale : Real_Scaling.Object;

      Png_Xmax : Natural;
      Png_Ymax : Natural;

      -------------
      -- Terrain --
      -------------

      function Terrain (Xl, Xr : X_Real; Yb, Yt : Y_Real) return Terrains is
         First : Boolean  := True;
         Terr  : Terrains := Default_Terrain;

         Rt    : constant Coordinate'Base :=
                   Coordinate'Base (Real (Scale.Scale_Y (Real (Yt))));
         Rb    : constant Coordinate'Base :=
                   Coordinate'Base (Real (Scale.Scale_Y (Real (Yb))));

         Cl    : constant Coordinate'Base :=
                   Coordinate'Base (Real (Scale.Scale_X (Real (Xl))));
         Cr    : constant Coordinate'Base :=
                   Coordinate'Base (Real (Scale.Scale_X (Real (Xr))));
         use Agpl.Conversions;
      begin
--           Log ("(Xl-Xr, Yb-Yt)-(Cl-Cr,Rb-Rt):" &
--                "(" &
--                S (Float (Xl)) & "-" & S (Float (Xr)) & "," &
--                S (Float (Yb)) & "-" & S (Float (Yt)) & ")-(" &
--                S (Float (Scale.Scale_X (Real (Xl)))) & "-" &
--                S (Float (Scale.Scale_X (Real (Xr)))) & "," &
--                S (Float (Scale.Scale_Y (Real (Yt)))) & "-" &
--                S (Float (Scale.Scale_Y (Real (Yb)))) & ")",
--                Always);
         --  The 'Max in the loop are to ensure that, even with rounding at small
         --  sizes, at least one pixel is inspected
         for R in Rt .. Integer'Max (Rt, Rb - 1) loop
            for C in Cl .. Integer'Max (Cl, Cr - 1) loop
--                 Log ("RCV" & R'Img & C'Img & " " &
--                      F.Red_Value (R, C)'Img &
--                      F.Green_Value (R, C)'Img &
--                      F.Blue_Value (R, C)'Img &
--                      F.Avg_Value (R, C)'Img, Always);
               declare
                  Local_Terr : Terrains := Default_Terrain;
               begin
                  if R >= 0 and then R <= Png_Ymax and then
                     C >= 0 and then C <= Png_Xmax
                  then
                     Local_Terr := T (F.Avg_Value (R, C));
                  end if;

                  if First then
                     First := False;
                     Terr  := Local_Terr;
                  elsif Terr /= Local_Terr then
                     return Mixed;
                  end if;
               end;
            end loop;
         end loop;

         return Terr;
      exception
         when E : others =>
            --  Happens once for some maps, no idea but I suspect of bug in Png_IO
            Log ("Error bounds : " & Cl'Img & Cr'Img & Rt'Img & Rb'Img,
                 Warning, Log_Section);
            Log ("Error is: " & Report (E), Warning, Log_Section);
            return Mixed;
      end Terrain;

   begin
      Open (F, File);

      if Buggy_Stage then
         Scale := Real_Scaling.Set_Equivalence
           (Real (Xmin),           Real (Xmax),
            Real (Ymin),           Real (Ymax),
            0.0,                   Real (Width (F)),
            Real (Height (F) - 1), 0.0);
      else
         Scale := Real_Scaling.Set_Equivalence
           (Real (Xmin),       Real (Xmax),
            Real (Ymin),       Real (Ymax),
            0.0,               Real (Width (F)),
            Real (Height (F)), 0.0);
      end if;

      Png_Xmax := Width (F)  - 1;
      Png_Ymax := Height (F) - 1;

      Log ("Scale extremes:" &
           Coordinate (Scale.Scale_X (Real (Xmin)))'Img &
           Coordinate (Scale.Scale_X (Real (Xmax)))'Img &
           Coordinate (Scale.Scale_Y (Real (Ymin)))'Img &
           Coordinate (Scale.Scale_Y (Real (Ymax)))'Img,
           Debug, log_section);

      declare
         procedure Png_Create is new Qtree.Create (Terrain);
      begin
         Png_Create (This, Xmin, Xmax, Ymin, Ymax, Id);
      end;

   end From_Png;

   -----------------
   -- From_Bitmap --
   -----------------

   procedure From_Bitmap (This        : in out Object;
                          Bmp         :        Bitmap.Object;
                          Xmin, Xmax  :        X_Real;
                          Ymin, Ymax  :        Y_Real;
                          Id          :        String := Default_Map_Id)
   is

      T : constant array (Bitmap.Terrains) of Terrains :=
            (Bitmap.Free => Free, Bitmap.Obstacle => Obstacle);

      package Real_Scaling is new Agpl.Scaling2d (Types.Real);
      Scale : Real_Scaling.Object;

      -------------
      -- Terrain --
      -------------

      function Terrain (Xl, Xr : X_Real; Yb, Yt : Y_Real) return Terrains is
         First : Boolean  := True;

         Terr  : Terrains := Default_Terrain;

         Rt    : constant Ordinate :=
                   Ordinate (Real(Scale.Scale_Y (Real (Yt))));
         Rb    : constant Ordinate :=
                   Ordinate (Real (Scale.Scale_Y (Real (Yb))));

         Cl    : constant Abscissa :=
                   Abscissa (Real (Scale.Scale_X (Real (Xl))));
         Cr    : constant Abscissa :=
                   Abscissa (Real (Scale.Scale_x (Real (Xr))));
         use Agpl.Conversions;
      begin
         --  The 'Max in the loop are to ensure that, even with rounding at small
         --  sizes, at least one pixel is inspected
         for R in Rt .. Rb - 1 loop
            for C in Cl .. Cr - 1 loop
               declare
                  Local_Terrain : Terrains := Default_Terrain;
               begin
                  if Bmp.Within_Bounds (C, R) then
                     Local_Terrain := T (Bmp.Get_At (C, R));
                  end if;

                  if First then
                     First := False;
                     Terr  := Local_Terrain;
                  elsif Terr /= Local_Terrain then
                     return Mixed;
                  end if;
               end;
            end loop;
         end loop;

         return Terr;
      exception
         when E : others =>
            Log ("Error bounds : " & Cl'Img & Cr'Img & Rt'Img & Rb'Img,
                 Warning, Log_Section);
            Log ("Error is: " & Report (E), Warning, Log_Section);
            return Mixed;
      end Terrain;

   begin
      Scale := Real_Scaling.Set_Equivalence
        (Real (Xmin),          Real (Xmax),
         Real (Ymin),          Real (Ymax),
         Real (Bmp.Get_X_Min), Real (Bmp.Get_X_Max),
         Real (Bmp.Get_Y_Min), Real (Bmp.Get_Y_Max));

      Log ("Scale extremes:" &
           Abscissa (Scale.Scale_X (Real (Xmin)))'Img &
           Abscissa (Scale.Scale_X (Real (Xmax)))'Img &
           Ordinate (Scale.Scale_Y (Real (Ymin)))'Img &
           Ordinate (Scale.Scale_Y (Real (Ymax)))'Img,
           Debug, log_section);

      declare
         procedure Bmp_Create is new Qtree.Create (Terrain);
      begin
         Bmp_Create (This, Xmin, Xmax, Ymin, Ymax, Id);
      end;
   end From_Bitmap;

end Sancta.Map.Qtree.Builder;
