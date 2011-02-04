with Agpl.Conversions;

package body Sancta.Types is

   Pi     : constant Real := Ada.Numerics.Pi;
   Two_Pi : constant Real := 2.0 * Pi;

   --------------
   -- To_Angle --
   --------------

   function To_Angle (A : in Float) return Angle is
   begin
      return Angle (Normalize_Zero (Real (A)));
   end To_Angle;

   function To_Angle (A : in Real)  return Angle is
   begin
      return Angle (Normalize_Zero (A));
   end To_Angle;

   --------------
   -- To_Point --
   --------------

   function To_Point (P : Pose) return Point is
   begin
      return (X => P.X, Y => P.Y);
   end To_Point;

   ------------------------
   -- Normalize_Positive --
   ------------------------

   function Normalize_Positive (Angle : in Real) return Real is
      A      : Real     := Angle;
   begin
      while A < 0.0 loop
         A := A + Two_Pi;
      end loop;
      while A > Two_Pi loop
         A := A - Two_Pi;
      end loop;

      return A;
   end Normalize_Positive;

   --------------------
   -- Normalize_Zero --
   --------------------

   function Normalize_Zero (Angle : in Real) return Real is
      A      : Real     := Angle;
   begin
      while A < -Pi loop
         A := A + Two_Pi;
      end loop;
      while A > Pi loop
         A := A - Two_Pi;
      end loop;

      return A;
   end Normalize_Zero;

   ---------
   -- "+" --
   ---------

   function "+" (L, R : in Pose) return Pose is
   begin
      return (L.X + R.X,
              L.Y + R.Y,
              L.A + R.A);
   end "+";

   function "-" (L, R : in Pose) return Pose is
   begin
      return (L.X - R.X,
              L.Y - R.Y,
              L.A - R.A);
   end "-";

   ---------
   -- "/" --
   ---------

   function "/" (L : Pose; R : Real) return Pose is
   begin
      return (L.X / R,
              L.Y / R,
              L.A / Angle (R));
   end "/";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : in Angle) return Angle is
      A : Real := Real (L) + Real (R);
   begin
      while A < Pi loop
         A := A + Two_Pi;
      end loop;
      while A > Pi loop
         A := A - Two_Pi;
      end loop;

      return Angle (A);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : in Angle) return Angle is
   A : Real := Real (L) - Real (R);
   begin
      while A < Pi loop
         A := A + Two_Pi;
      end loop;
      while A > Pi loop
         A := A - Two_Pi;
      end loop;

      return Angle (A);
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (X : in Float) return Types.Real is
   begin
      return Real (X);
   end "+";

   function "+" (X : in Agpl.Cv.Point2D) return Types.Pose is
   begin
      return (Real (X(1)), Real (X(2)), 0.0);
   end "+";

   function "+" (X : in Types.Real) return Float is
   begin
      return Float (X);
   end "+";

   function "+" (X : in Types.Pose) return Agpl.Cv.Point2D is
   begin
      return (+X.X, +X.Y, 1.0);
   end "+";

   function "+" (X : in Types.Angle) return Types.Real is
   begin
      return Real (X);
   end "+";

   function "+" (X : in X_Coordinate) return Integer is
   begin
      return Integer (X);
   end "+";

   function "+" (X : in Y_Coordinate) return Integer is
   begin
      return Integer (X);
   end "+";

   -----------------
   -- Agent_Names --
   -----------------

   function Agent_Names return Agent_Name_Array is
   begin
      return
        ('a' => Value ("Ari"),
         'b' => Value ("Ben"),
         'c' => Value ("Ced"),
         'd' => Value ("Dan"),
         'e' => Value ("Epi"),
         'f' => Value ("Fez"),
         'g' => Value ("Ged"),
         'h' => Value ("Hop"),
         'z' => Value ("Zak"),
         others => Value ("Xxx"));
   end Agent_Names;

   -----------
   -- Image --
   -----------

   function Image (P : Pose; Decimals : Natural := 2) return String is
      function S is new Agpl.Conversions.To_Str (Real);
      function S is new Agpl.Conversions.To_Str (Angle);
   begin
      return S (P.X, Decimals) & " " & S (P.Y, Decimals) & " " & S (P.A, Decimals);
   end Image;

end Sancta.Types;
