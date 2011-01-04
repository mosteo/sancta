package body Sancta.Ctree.Single is

   use type Sancta.Costs;
   use type Sancta.Map.Location'Class;
   use type Sancta.Map.Paths.Cursor;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Map       :     Sancta.Map.Object'Class;
      Navigator :     Tree_Navigator.Object'Class;
      Order     :     Tc.Lists.List;
      Bots      :     Positive;
      Limit     :     Network_Range;
      Minsum    : out Sancta.Costs;
      Minmax    : out Sancta.Costs;
      Minave    : out Sancta.Costs)
   is
      Pos         : Sancta.Map.Paths.Cursor;

      Backloc     : Sancta.Map.Paths.Cursor;
      --  Both back point to the same location, but in the two branches.
      --  So the location is actually the same, but the cursors aren't!

      Remaining   : Tc.Lists.List := Order;
      Depth       : Sancta.Costs  := 0.0;
      Step        : Sancta.Costs;
      Branch      : Sancta.Map.Path;
      Moving      : Positive;

      use Sancta.Map.Paths;

      procedure Report is
      begin
         Log ("Depth" & Depth'Img & "; " &
              "Moving" & Moving'Img & "; " &
              "Remain" & Remaining.Length'Img & "; " &
              "Sum" & Minsum'Img & "; " &
              "Max" & Minmax'Img & "; " &
              "Ave" & Minave'Img,
              Debug, Log_Section);
      end Report;
   begin
      Minsum := 0.0;
      Minmax := 0.0;
      Minave := 0.0;

      Branch := Navigator.Branch (Order.First_Element);
      Pos    := Branch.First;

      while not Remaining.Is_Empty loop
         --  GOING DOWN
         Log ("GOING DOWN", Debug, Log_Section);
         while Pos /= Branch.Last loop
            Step := Map.Get_Cost_Between
              (Element (Pos),
               Element (Next (Pos)));

            Depth  := Depth  + Step;

            Moving := Bots - Natural (Float'Floor (Float (Depth) / Limit));

            Minmax := Minmax + Step;
            Minsum := Minsum + Step * Sancta.Costs (Moving);

            Pos    := Next (Pos);

            Report;
         end loop;

         --  TASK REACHED
         Log ("TASK REACHED", Debug, Log_Section);
         Remaining.Delete_First;
         Minave := Minave + Minmax;
         Report;

         --  GOING UP
         Log ("GOING UP", Debug, Log_Section);
         if Remaining.Is_Empty then
            Backloc     := Branch.First;
         else
            Backloc     := Sancta.Map.Common_Ancestor
              (Branch,
               Navigator.Branch (Remaining.First_Element));
         end if;

         while Pos /= Backloc loop
            Step := Map.Get_Cost_Between
              (Element (Previous (Pos)),
               Element (Pos));

            Moving := Bots - Natural (Float'Floor (Float (Depth) / Limit));

            Depth  := Depth  - Step;
            Minmax := Minmax + Step;
            Minsum := Minsum + Step * Sancta.Costs (Moving);

            Pos    := Previous (Pos);

            Report;
         end loop;

         Log ("BACKTRACK ENDED", Debug, Log_Section);
         if not Remaining.Is_Empty then
            declare
               Loc : constant Sancta.Map.Location'Class := Element (Pos);
            begin
               Branch := Navigator.Branch (Remaining.First_Element);
               Pos    := Branch.Find (Loc);
            end;
         else
            Log ("MISSION ENDED", Debug, Log_Section);
         end if;
      end loop;

      Minave := Minave / Sancta.Costs (Order.Length);

      Report;
   end Evaluate;

end Sancta.Ctree.Single;
