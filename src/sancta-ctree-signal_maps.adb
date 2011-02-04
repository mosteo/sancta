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

         This.Count.Include (At_Loc, This.Count.Element (At_Loc) + 1);
      end Increment;

      procedure Store (Loc_1, Loc_2 : Map.Location'Class; Q : Signal_Q) is
         use Location_Samples;
         I_1 : constant Cursor := This.Samples.Find (Loc_1);
         I_2 : constant Cursor := This.Samples.Find (Loc_2);

         Samples : Pair_Sample_Access;
      begin
         if Has_Element (I_1) and then Element (I_1).Contains (Loc_2) then
            --  Easy, go ahead -- efficiently
            Samples := Element (I_1).Element (Loc_2);
         else
            Samples := new Pair_Sample'(Pair (Loc_1, Loc_2), Q_Lists.Empty_List);

            --  Someone is missing, fill them both completely
            if not Has_Element (I_1) then
               declare
                  X : constant Pair_Samples_Map_Access := new Pair_Samples.Map;
               begin
                  X.Insert (Loc_2, Samples);
                  This.Samples.Insert (Loc_1, X);
               end;
            end if;

            if not Has_Element (I_2) then
               declare
                  X : constant Pair_Samples_Map_Access := new Pair_Samples.Map;
               begin
                  X.Insert (Loc_1, Samples);
                  This.Samples.Insert (Loc_2, X);
               end;
            end if;

            pragma Assert (This.Samples.Element (Loc_1).Element (Loc_2) =
                             This.Samples.Element (Loc_2).Element (Loc_1));
         end if;

         pragma Assert (Samples /= null);
         --  At this point, it points to a valid sample list shared by both locs.
         pragma Assert (Samples.Pair = Pair (Loc_1, Loc_2));

         Samples.Samples.Append (Q);

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

end Sancta.Ctree.Signal_Maps;
