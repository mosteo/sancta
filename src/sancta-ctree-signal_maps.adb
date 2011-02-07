package body Sancta.Ctree.Signal_Maps is

   ------------
   -- Create --
   ------------

   procedure Create
     (This : in out Map_Family;
      Over : Sancta.Map.Smart.Object)
   is
   begin
      This.Map := Over;
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
      use Sancta.Map;

      Loc_1 : constant Map.Location'Class :=
                Nearest_Location (This.Map.Ref.all, Pos_1);
      Loc_2 : constant Map.Location'Class :=
                Nearest_Location (This.Map.Ref.all, Pos_2);

      procedure Increment (At_Loc : Map.Location'Class) is
      begin
         if not This.Count.Contains (At_Loc) then
            This.Count.Insert (At_Loc, 0);
         end if;

         declare
            Count : constant Natural := This.Count.Element (At_Loc);
         begin
            This.Count.Include (At_Loc, Count + 1);
            This.Most_Sampled_Loc :=
              Natural'Max (This.Most_Sampled_Loc, Count + 1);
         end;
      end Increment;

      procedure Store (Loc_1, Loc_2 : Map.Location'Class; Q : Signal_Q) is
         use Location_Samples;
         I_1 : constant Cursor := This.Samples.Find (Loc_1);
         I_2 : constant Cursor := This.Samples.Find (Loc_2);

         Samples : Pair_Sample_Access;
      begin
         Log ("Adding sample to Pair Loc_1:Loc_2: " &
              Pair (Loc_1, Loc_2).Image, Always);

         if Has_Element (I_1) and then Element (I_1).Contains (Loc_2) then
            --  Easy, go ahead -- efficiently
            Samples := Element (I_1).Element (Loc_2);
         else
            Samples := new Pair_Sample'(Pair (Loc_1, Loc_2), Q_Lists.Empty_List);

            --  Someone is missing, fill them both completely
            declare
               L_1_2 : Pair_Samples_Map_Access;
            begin
               if Has_Element (I_1) then
                  Log ("Reusing Loc_1.Loc_2.Map", Always);
                  L_1_2 := Element (I_1);
               else
                  Log ("Adding Loc_1.Loc_2.Map", Always);
                  L_1_2 := new Pair_Samples.Map;
                  This.Samples.Insert (Loc_1, L_1_2);
               end if;

               L_1_2.Insert (Loc_2, Samples);
            end;

            --  Symmetric access:
            declare
               L_2_1 : Pair_Samples_Map_Access;
            begin
               if Has_Element (I_2) then
                  Log ("Reusing Loc_2.Loc_1.Map", Always);
                  L_2_1 := Element (I_2);
               else
                  Log ("Adding Loc_2.Loc_1.Map", Always);
                  L_2_1 := new Pair_Samples.Map;
                  This.Samples.Insert (Loc_2, L_2_1);
               end if;

               L_2_1.Insert (Loc_1, Samples);
            end;

            pragma Assert (This.Samples.Element (Loc_1).Element (Loc_2) =
                             This.Samples.Element (Loc_2).Element (Loc_1),
                           "Improper insertion in signal map");
         end if;

         pragma Assert (Samples /= null);
         --  At this point, it points to a valid sample list shared by both locs.
         pragma Assert (Samples.Pair = Pair (Loc_1, Loc_2));

         Samples.Samples.Append (Q);
         This.Most_Sampled_Pair := Natural'Max
           (This.Most_Sampled_Pair, Natural (Samples.Samples.Length));

      end Store;

   begin
      if Loc_1 = Loc_2 then
         return;
      end if;

      Increment (Loc_1);
      Increment (Loc_2);

      Store (Loc_1, Loc_2, Q);
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

   ---------
   -- "<" --
   ---------

--     function "<" (L, R : Location_Pair) return Boolean is
--        use type Map.Location'Class;
--        function "<" (L, R : Map.Location'Class) return Boolean
--                      renames Map.Less_Than;
--     begin
--        return L.First_Element < R.First_Element or else
--          (L.First_Element = R.First_Element and then
--           L.Last_Element < R.Last_Element);
--     end "<";

   procedure Print (This : Map_Family) is
      Pairs_Count : Natural := 0;
      use Location_Samples;
      procedure Count (I : Cursor) is
      begin
         Pairs_Count := Pairs_Count + Natural (Element (I).Length);
      end Count;
   begin
      This.Samples.Iterate (Count'Access);
      Pairs_Count := Pairs_Count / 2;

      Log ("MAP FAMILY DEBUG DUMP START ********************",              Always, Log_Section);
      Log ("Locations with samples:" & This.Count.Length'Img,               Always, Log_Section);
      Log ("Location pairs:" & Pairs_Count'Img,                             Always, Log_Section);
      Log ("Samples at most sampled location:" & This.Most_Sampled_Loc'Img, Always, Log_Section);
      Log ("Samples at most sampled pair:" & This.Most_Sampled_Pair'Img,    Always, Log_Section);
      Log ("MAP FAMILY DEBUG DUMP END   ********************", Always);
   end Print;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Density_View;
                   D    : in out Agpl.Drawing.Drawer'Class) is
   begin
      null;
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Quality_View;
                   D    : in out Agpl.Drawing.Drawer'Class) is
   begin
      null;
   end Draw;

end Sancta.Ctree.Signal_Maps;
