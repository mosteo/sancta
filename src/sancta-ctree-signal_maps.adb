with Ada.Numerics.Elementary_Functions;

with Agpl.Constants;
with Agpl.Drawing.Transformations;
with Agpl.Scaling1D;
with Agpl.Strings;
with Agpl.Types;

with Sancta.Map.Qtree;

package body Sancta.Ctree.Signal_Maps is

   ---------------------
   -- Map_Family_Safe --
   ---------------------

   protected body Map_Family_Safe is

      ------------
      -- Create --
      ------------

      procedure Create (Over : Sancta.Map.Smart.Object)
      is
      begin
         The_Map := Over;
      end Create;

      ---------------------
      -- Add_Observation --
      ---------------------

      procedure Add_Observation
        (Pos_1 :        Types.Point;
         Pos_2 :        Types.Point;
         Q     :        Signal_Q)
      is
         use Sancta.Map;

         Loc_1 : constant Map.Location'Class :=
                   Nearest_Location (The_Map.Ref.all, Pos_1);
         Loc_2 : constant Map.Location'Class :=
                   Nearest_Location (The_Map.Ref.all, Pos_2);

         procedure Increment (At_Loc : Map.Location'Class) is
         begin
            if not Count.Contains (At_Loc) then
               Count.Insert (At_Loc, 0);
            end if;

            declare
               C : constant Natural := Count.Element (At_Loc);
            begin
               Count.Include (At_Loc, C + 1);
               Most_Sampled_Loc := Natural'Max (Most_Sampled_Loc, C + 1);
            end;
         end Increment;

         procedure Store (Loc_1, Loc_2 : Map.Location'Class; Q : Signal_Q) is
            use Location_Samples;
            I_1 : constant Cursor := Samples.Find (Loc_1);
            I_2 : constant Cursor := Samples.Find (Loc_2);

            Sample : Pair_Sample_Access;
         begin
            Log ("Adding sample to Pair Loc_1:Loc_2: " &
                 Pair (Loc_1, Loc_2).Image, Debug, Log_Section);

            if Has_Element (I_1) and then Element (I_1).Contains (Loc_2) then
               --  Easy, go ahead -- efficiently
               Sample := Element (I_1).Element (Loc_2);
            else
               Sample :=
                 new Pair_Sample'(Pair (Loc_1, Loc_2), Samples => <>);

               --  Someone is missing, fill them both completely
               declare
                  L_1_2 : Pair_Samples_Map_Access;
               begin
                  if Has_Element (I_1) then
                     Log ("Reusing Loc_1.Loc_2.Map", Debug, Log_Section);
                     L_1_2 := Element (I_1);
                  else
                     Log ("Adding Loc_1.Loc_2.Map", Debug, Log_Section);
                     L_1_2 := new Pair_Samples.Map;
                     Samples.Insert (Loc_1, L_1_2);
                  end if;

                  L_1_2.Insert (Loc_2, Sample);
               end;

               --  Symmetric access:
               declare
                  L_2_1 : Pair_Samples_Map_Access;
               begin
                  if Has_Element (I_2) then
                     Log ("Reusing Loc_2.Loc_1.Map", Debug, Log_Section);
                     L_2_1 := Element (I_2);
                  else
                     Log ("Adding Loc_2.Loc_1.Map", Debug, Log_Section);
                     L_2_1 := new Pair_Samples.Map;
                     Samples.Insert (Loc_2, L_2_1);
                  end if;

                  L_2_1.Insert (Loc_1, Sample);
               end;

               pragma Assert (Samples.Element (Loc_1).Element (Loc_2) =
                              Samples.Element (Loc_2).Element (Loc_1),
                              "Improper insertion in signal map");
            end if;

            pragma Assert (Sample /= null);
            --  At this point, it points to a valid sample list shared by both locs.
            pragma Assert (Sample.Pair = Pair (Loc_1, Loc_2));

            --  Here, actual sample computation:
            --  Cache avg for fast display:
            Sample.Samples.Append (+Q);

            Most_Sampled_Pair :=
              Natural'Max (Most_Sampled_Pair, Sample.Samples.Length);

         end Store;

      begin
         if Loc_1 = Loc_2 then
            return;
         end if;

         Increment (Loc_1);
         Increment (Loc_2);

         Store (Loc_1, Loc_2, Q);
      end Add_Observation;

      ------------------
      -- Draw_Density --
      ------------------

      procedure Draw_Density (Into : in out Agpl.Drawing.Drawer'Class)
      is
         use Ada.Numerics.Elementary_Functions;

         package Scale is new Agpl.Scaling1d (Float);

         Gradient : constant Scale.Object := Scale.Set_Equivalence
           (0.0, Log (Float'Max (10.0, Float (Most_Sampled_Loc)), 10.0),
            255.0, 0.0);

         use Agpl.Constants;
         use Agpl.Types;
         use Location_Count_Maps;
         procedure Draw (I : Cursor) is
            Cell   : constant Map.Qtree.Cell_Coords :=
                       Map.Qtree.Location (Key (I)).Coords;
            Count  : constant Natural := Element (I);
            Blue   : constant Unsigned_8 :=
                       Unsigned_8 (Gradient.Scale (Log (Float (Count), 10.0)));
         begin
            Into.Set_Color ((Blue, Blue, 255), Alpha_Opaque);
            Into.Fill_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));
            Into.Set_Color (Black, Alpha_Opaque);
            Into.Draw_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));

            declare
               use Agpl.Strings;
               Fit : Agpl.Drawing.Transformations.Transformer;
            begin
               Fit.Set_Color (Black, Alpha_Opaque);
               Fit.Write (0.0, 0.0, To_String (Count));
               Fit.Fit (Into,
                        Float (Cell.Xl), Float (Cell.Xr),
                        Float (Cell.Yb), Float (Cell.Yt),
                        Square => True);
            end;
         end Draw;
      begin
         Count.Iterate (Draw'Access);
      end Draw_Density;

      ------------------
      -- Draw_Quality --
      ------------------

      procedure Draw_Quality (From :        Map.Location'Class;
                              Into : in out Agpl.Drawing.Drawer'Class)
      is
         pragma Precondition (Ref_Loc.Is_Valid);

         use Agpl.Constants;
         use Agpl.Types;

         package Scale is new Agpl.Scaling1d (Float);
         Gradient_Red : constant Scale.Object := Scale.Set_Equivalence
           (0.0, Float (Parent.Threshold),
            16.0, 255.0);
         Gradient_Green : constant Scale.Object := Scale.Set_Equivalence
           (Float (Parent.Threshold), Float (Signal_Q'Last),
            184.0, 0.0);

         Base : constant Map.Qtree.Cell_Coords :=
                    Map.Qtree.Location (Ref_Loc.Get).Coords;

         use Pair_Samples;
         procedure Draw (I : Cursor) is
            Cell   : constant Map.Qtree.Cell_Coords :=
                       Map.Qtree.Location (Key (I)).Coords;
            Sample : constant Pair_Sample_Access := Element (I);
            Tint   : Unsigned_8;
            Value  : constant Signal_Q := +Sample.Samples.Mean;
         begin
            if Value < Parent.Threshold then
               Tint := Unsigned_8 (Gradient_Red.Scale (Float (Value)));
               Into.Set_Color ((255, Tint, Tint), Alpha_Opaque);
            else
               Tint := Unsigned_8 (Gradient_Green.Scale (Float (Value)));
               Into.Set_Color ((Tint, 200, Tint), Alpha_Opaque);
            end if;
            Into.Fill_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));
            Into.Set_Color (Black, Alpha_Opaque);
            Into.Draw_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));

            --  Whiskers
            declare
               Fit : Agpl.Drawing.Transformations.Transformer;
            begin
               Fit.Set_Color (Red, Alpha_Opaque);
               Fit.Draw_Line (-Float (Signal_Q'Last) / 2.0, Float (Parent.Threshold),
                              +Float (Signal_Q'Last) / 2.0, Float (Parent.Threshold));
               Sample.Samples.Drawable (Width => 0.2).Draw (Fit);
               Fit.Fit (Into,
                        Float (Cell.Xl), Float (Cell.Xr),
                        Float (Cell.Yb), Float (Cell.Yt),
                        Square => True);
            end;
         end Draw;

      begin
         if Samples.Contains (Ref_Loc.Get) then
            --  Draw rest
            Samples.Element (Ref_Loc.Get).Iterate (Draw'Access);
         end if;

         --  Draw reference location
         Into.Set_Color ((0, 200, 0), Alpha_Opaque);
         Into.Fill_Rectangle
           (Float (Base.Xl), Float (Base.Yb),
            Float (Base.Xr), Float (Base.Yt));
         Into.Set_Color (Black, Alpha_Opaque);
         Into.Draw_Line
           (Float (Base.Xl), Float (Base.Yb),
            Float (Base.Xr), Float (Base.Yt));
         Into.Draw_Line
           (Float (Base.Xr), Float (Base.Yb),
            Float (Base.Xl), Float (Base.Yt));
      end Draw_Quality;

      -----------------
      -- Draw_Decide --
      -----------------

      procedure Draw_Decide  (Into : in out Agpl.Drawing.Drawer'Class) is
      begin
         if Ref_Loc.Is_Valid then
            Draw_Quality (From => Ref_Loc.Ref.all, Into => Into);
         else
            Draw_Density (Into);
         end if;
      end Draw_Decide;

      -------------
      -- Clicked --
      -------------

      procedure Clicked (P : Types.Point) is
      begin
         declare
            use type Map.Location'Class;
            Loc : constant Map.Location'Class := The_Map.Ref.Nearest_Location (P);
         begin
            if Ref_Loc.Is_Valid and then Loc = Ref_Loc.Get then
               Ref_Loc.Clear;
               Log ("Clearing reference location: " & Loc.Image,
                    Informative, Log_Section);
            else
               Ref_Loc.Set (Loc);
               Log ("Setting new reference location: " & Loc.Image,
                    Informative, Log_Section);
            end if;
         end;
      exception
         when E : others =>
            Log ("Cannot find clicked location: " & Report (E), Warning);
      end Clicked;

   end Map_Family_Safe;

   ------------
   -- Create --
   ------------

   procedure Create
     (This : in out Map_Family;
      Over : Sancta.Map.Smart.Object;
      Q_T  :        Signal_Q)
   is
   begin
      This.Safe.Create (Over);
      This.Threshold := Q_T;
   end Create;

   ---------------------
   -- Add_Observation --
   ---------------------

   procedure Add_Observation
     (This  : in out Map_Family;
      Pos_1 :        Types.Point;
      Pos_2 :        Types.Point;
      Q     :        Signal_Q)
   is
   begin
      This.Safe.Add_Observation (Pos_1, Pos_2, Q);
   end Add_Observation;

   ----------
   -- Pair --
   ----------

   function Pair (L, R : Map.Location'Class) return Location_Pair is
      P : Location_Pair;
   begin
      P.Insert (L);
      P.Insert (R);
      return P;
   end Pair;

   -----------
   -- Image --
   -----------

   function Image (Pair : Location_Pair) return String is
   begin
      return "PAIR: " & Pair.First_Element.Image & "::" & Pair.Last_Element.Image;
   end Image;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Quality_View;
                   D    : in out Agpl.Drawing.Drawer'Class) is
   begin
      This.Parent.Safe.Draw_Decide (Into => D);
   end Draw;

   ------------
   -- Create --
   ------------

   function Create (From : Map_Family'Class) return Quality_View is
   begin
      return (Parent => From.Self);
   end Create;

   ---------------
   -- Triggered --
   ---------------

   procedure Triggered (Handler : in out Quality_View; E : Agpl.Gui.Event'Class)
   is
      use Sancta.Types;
      Click : Agpl.Gui.Clicked'Class renames Agpl.Gui.Clicked'Class (E);
   begin
       Handler.Parent.Safe.Clicked ((+Click.Data_X, +Click.Data_Y));
   end Triggered;

   ---------
   -- "+" --
   ---------

   function "+" (Q : Signal_Q) return Float_Q is
   begin
      return Float_Q (Q);
   end "+";

   function "+" (F : Float_Q)  return Signal_Q is
   begin
      return Signal_Q (F);
   end "+";

end Sancta.Ctree.Signal_Maps;
