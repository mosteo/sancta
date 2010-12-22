with Ada.Numerics;

with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Types.Real_Math;
with Sancta.Types.Transformations;

with Agpl.Cv;
with Agpl.Conversions; use Agpl.Conversions;

package body Sancta.Types.Operations is

   package Math renames Real_Math;
   use Agpl.Constants;
   use Transformations.Real_Transf;

   ---------------------------
   -- Best_Observation_Pose --
   ---------------------------
   --  See notes for the details of the calculations.
   function Best_Observation_Pose (A, B : in Pose;
                                   FOV  : in Float) return Pose
   is
      use Math;
      use Real_Transf;
      FOV_2        : constant Real := Real (FOV / 2.0);
      Half_Horizon : constant Real := Distance (A, B) / 2.0;
      Dist         : constant Real := Half_Horizon * Cot (FOV_2);
      --  Distance to the AB segment.
      Frame        : constant Real_Transf.Pose := (A.X,
                                                   A.Y,
                                                   Arctan (B.Y - A.Y,
                                                           B.X - A.X), 1.0);
      --  This represents the frame reference where the gap is origin.
      Target       : constant Real_Transf.Pose :=
                       Compose (Frame, (Half_Horizon, -Dist, Pi_2, 1.0));
   begin
      return (Target (1), Target (2), To_Angle (Target (3)));
   end Best_Observation_Pose;

   --------------
   -- Distance --
   --------------

   function Distance (A, B : in Pose) return Real is
      use Real_Math;
   begin
      return Sqrt ((A.X - B.X) * (A.X - B.X) +
                   (A.Y - B.Y) * (A.Y - B.Y));
   end Distance;

   -------------
   -- Modulus --
   -------------

   function Modulus (P : in Pose) return Real is
   begin
      return Math.Sqrt (P.X * P.X + P.Y * P.Y);
   end Modulus;

   ----------
   -- Time --
   ----------

   function Time (A, B        : in Pose;
                  Lin_Speed   : in Real;
                  Ang_Speed   : in Real;
                  Use_A_Angle : in Boolean := True;
                  Use_B_Angle : in Boolean := True)
      return Real
   is
   begin
      if A.X = B.X and then A.Y = B.Y then
         if Use_A_Angle and then Use_B_Angle then
            return Real (abs (A.A - B.A)) / Ang_Speed; -- Only turn in place.
         else
            return 0.0;
         end if;
      else
         declare
            Travel_A : constant Angle :=
              Angle (Math.Arctan (B.Y - A.Y, B.X - A.X));
            Total    :          Real  := 0.0;
         begin
            Total := Total + Distance (A, B) / Lin_Speed; -- Linear travel time
            if Use_A_Angle then
               Total := Total + Real (abs (Travel_A - A.A)) / Ang_Speed;
               --  First rotation.
            end if;
            if Use_B_Angle then
               Total := Total + Real (abs (B.A - Travel_A)) / Ang_Speed;
               --  Second rotation.
            end if;

            return Total;
         end;
      end if;
   exception
      when E : others =>
         Log ("Operations.Time: " & Report (E), Warning);
         Log ("Arguments: " &
              To_String (A) & "; " & To_String (B), Warning);
         return 0.0;
   end Time;

   ------------
   -- Offset --
   ------------

   function Offset (P, Q : Pose) return Angle is
      A : constant Angle := To_Angle (Real_Math.Arctan (Q.Y - P.Y, Q.X - P.X));
   begin
      return A - P.A;
   end Offset;

   -------------
   -- To_Pose --
   -------------

   function To_Pose (P : in Real_Transf.Pose) return Types.Pose is
   begin
      return (+P (1), +P (2), Angle (P (3)));
   end To_Pose;

   function To_Pose (P : in Types.Pose) return Real_Transf.Pose is
   begin
      return (P.X, P.Y, Real (P.A), 1.0);
   end To_Pose;

   ------------------
   -- Extract_Gaps --
   ------------------

   --  The algorithm works as follows: each two consecutive laser readings are
   --  linked by a imaginary line. If this line crosses the origin (with some
   --  differential margin) it means there's a gap. If the closer point is the
   --  first, the gap is unseen towards left.
   --  Out_of_range gaps happen between the first and last readings of a
   --  sequence where all the points have maximum range (i.e. no obstacle found).
   --  These gaps model the visibility horizon caused by distance instead of
   --  obstacles.

   function Extract_Gaps (This      : Range_Scan;
                          Epsilon   : Float := 0.35;
                          Min_Width : Float := 1.0;
                          Horizon   : Float := Float'Last;
                          Merge     : Merging_Policies := Always)
                          return Sancta.Gap.Object_Array
   is
      use Agpl.Cv;
      Gaps   : Gap.Object_Array (1 .. This'Length - 1); -- At worst one for each facet.
      Pos    : Positive := Gaps'First;
      Origin : constant Point2D := (0.0, 0.0, 1.0);
--      pragma Incomplete (Extract_Gaps, "Border gaps are not being computed");
      Closest : Float := Float'Last;
   begin
--        --  Starting gap if FOV < 2PI
--        if true then -- should be some proper condition but for now...
--           declare
--              P : constant Point2D := ( + This (This'First).X,
--                                        + This (This'First).Y, 1.0);
--           begin
--              if Distance (P, Origin) >= Min_Width then
--                 Gaps (Pos) := Gap.Create (Origin, P, Gap.Out_Of_Range);
--                 Pos        := Pos + 1;
--              end if;
--           end;
--        end if;

      for I in This'First .. This'Last - 1 loop
         declare
            use Transformations;
            PP1  : constant Types.Pose := Polar_To_Cart (This (I).A,
                                                         This (I).D);
            P1   : constant Point2D := (Float (PP1.X),
                                        Float (PP1.Y), 1.0);
            PP2  : constant Types.Pose := Polar_To_Cart (This (I + 1).A,
                                                         This (I + 1).D);
            P2   : constant Point2D := (Float (PP2.X),
                                        Float (PP2.Y), 1.0);
            Line : constant Line2D := Line2D (P1 ** P2);
         begin
            --  A new occlusive gap has been found:
            Closest := Float'Min (Closest, Agpl.Cv.Distance (Line, Origin));
            if Agpl.Cv.Distance (Line, Origin) <= Epsilon and then
               Agpl.Cv.Distance (P1, P2) >= Min_Width
            then
               Gaps (Pos) := Gap.Create (P1, P2, Gap.Occlusive);
               Pos        := Pos + 1;

            --  A new gap caused by readings being two apart.
            --  Event if they're not lined with the robot.
            elsif Agpl.Cv.Distance (P1, P2) >= Min_Width or else
                  Agpl.Cv.Distance (P1, Origin) >= Horizon or else
                  Agpl.Cv.Distance (P2, Origin) >= Horizon
            then
               Gaps (Pos) := Gap.Create (P1, P2, Gap.Out_Of_Range);
               Pos        := Pos + 1;
            end if;
         end;
      end loop;

      --  Ending gap if FOV < 2PI
--        if true then -- should be some proper condition but for now...
--           declare
--              P : constant Point2D := ( + This (This'Last).X,
--                                        + This (This'Last).Y, 1.0);
--           begin
--              if Distance (P, Origin) >= Min_Width then
--                 Gaps (Pos) := Gap.Create (P, Origin, Gap.Out_Of_Range);
--                 Pos        := Pos + 1;
--              end if;
--           end;
--        end if;

      --  Merging here
      if Merge = Always then
         declare
            use Agpl;
            use Gap;
            I : Integer := Pos - 2;
         begin
            while I >= Gaps'First loop
               --  Convex sequence of gaps to merge:
               if Get_End (Gaps (I)) = Get_Start (Gaps (I + 1)) and then
                 Signed_Distance
                   (Line2D (Get_Start (Gaps (I)) ** Get_End (Gaps (I + 1))),
                    Get_End (Gaps (I))) <= 0.0
               then
                  Gaps (I) := Create (Get_Start (Gaps (I)),
                                      Get_End   (Gaps (I + 1)),
                                      Out_Of_Range);

                  if I + 1 < Pos - 1 then
                     Gaps (I + 1 .. Pos - 2) := Gaps (I + 2 .. Pos - 1);
                  end if;

                  Pos := Pos - 1;
               end if;
               I := I - 1;
            end loop;
         end;
      end if;

      return Gaps (1 .. Pos - 1);
   end Extract_Gaps;

   -------------------
   -- Segment_Ratio --
   -------------------

   function Segment_Ratio (X, Y  : in Pose;
                           Ratio : in Float) return Pose
   is
      DX : constant Real  := Y.X - X.X;
      DY : constant Real  := Y.Y - X.Y;
      DA : constant Real  := Real (Y.A - X.A);
   begin
      return (X => X.X + DX * Real (Ratio),
              Y => X.Y + DY * Real (Ratio),
              A => Angle (Normalize_Zero (Real (X.A) + DA * Real (Ratio))));
   end Segment_Ratio;

   ---------------------
   -- Angle_From_Scan --
   ---------------------

   function Angle_From_Scan (Scan         : in Range_Scan;
                             Out_Of_Range : in Real) return Angle is
      use Math;
      type Angles_Array is array (0 .. 179) of Natural;
      Angles : Angles_Array := (others => 0);
      J      : Integer;
      P1, P2 : Types.Pose;
   begin
      for I in Scan'First .. Scan'Last - 32 loop
         --  Search another point that is at least 20 centimeters apart from the
         --  current one
         if Scan (I).D < Out_Of_Range then
            J := I + 1;
            while J <= Scan'Last loop
               P1  := Transformations.Polar_To_Cart (Scan (I).A, Scan (I).D);
               P2  := Transformations.Polar_To_Cart (Scan (J).A, Scan (J).D);
               declare
                  Gap : constant Real := Distance (P1, P2);
               begin
                  exit when Scan (J).D < Out_Of_Range and then Gap >= 0.2;
               end;
               J := J + 1;
            end loop;

            if J <= Scan'Last and then Scan (J).D < Out_Of_Range then
               declare
                  Ang : constant Types.Real :=
                          Normalize_Positive (Arctan (Y => P2.Y - P1.Y,
                                                      X => P2.X - P1.X));
                  Deg :          Integer := Integer (Ang * 180.0 / Pi);
               begin
                  if Distance (P1, P2) < 1.0 then -- lame filtering for impossible points
                     while Deg > 179 loop
                        Deg := Deg - 180;
                     end loop;
                     Angles (Deg) := Angles (Deg) + 1;
                  end if;
               end;
            end if;
         end if;
      end loop;

      declare
         Max  : Natural := 0;
         Best : Integer := 0;
      begin
         for I in Angles'Range loop
            if Angles (I) > 0 then
               Log ("A: " & To_String (Float (Real (I) * Pi / 180.0), 5) & ": " &
                    Angles (I)'Img, Debug, Section         => Log_Detail);
            end if;
            if Angles (I) > Max then
               Best := I;
               Max  := Angles (I);
            end if;
         end loop;

         if Angles (Best) > Scan'Length / 20 or else True then
            return Angle (Real (Best) * Pi / 180.0);
         else
            raise No_Angle;
         end if;
      end;
   exception
      when No_Angle =>
         raise;
      when E : others =>
         Log ("Operations.Angle_From_Scan: " & Report (E), Warning);
         raise No_Angle;
   end Angle_From_Scan;

   --------------------
   -- Corrected_Pose --
   --------------------

   function Corrected_Pose (Robot_Pose      : in Pose;
                            Angle_From_Scan : in Angle;
                            Guides          : in Angle := Angle (Pi_2);
                            Tolerance       : in Angle := 0.17) return Pose
   is
      A      : Real          := Normalize_Positive
        (Real (Robot_Pose.A) + Real (Angle_From_Scan));
      Result : Pose          := Robot_Pose;
      Bot_A  : constant Real := Real (Normalize_Zero (Real (Robot_Pose.A)));
   begin
      Log ("Robot says " & To_STring (Float (Bot_A), 5) &
           ", env says " & To_String (Float (A), 5),
           Debug, Section => Log_Section);

      while A > Real (Tolerance) loop
         A := A - Real (Guides);
      end loop;

      --  Now, either A is within tolerance or we have lost the opportunity.
      if abs (A) < Real (Tolerance) then
         Result.A := Result.A - Angle (A);

         Log ("Adjusting angle from " & To_String (Float (Robot_Pose.A), 5) &
                 " to " & To_String (Float (Result.A), 5),
                 Debug, Section => Log_Section);

         return Result;
      else
         raise No_Correction;
      end if;
   end Corrected_Pose;

   -------------------
   -- Oriented_Pose --
   -------------------

   function Oriented_Pose (Unoriented,
                           Oriented : Pose) return Pose
   is
   begin
      return (Unoriented.X, Unoriented.Y, Oriented.A);
   end Oriented_Pose;

   ----------------
   -- To_Radians --
   ----------------

   function To_Radians (Deg : Real) return Angle is
   begin
      return To_Angle (Deg * Ada.Numerics.Pi / 180.0);
   end To_Radians;

end Sancta.Types.Operations;
