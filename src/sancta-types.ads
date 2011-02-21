with Sancta;
with Agpl.Cv;
with Agpl.Containers.Naked_Vectors;
with Agpl.Protected_Value,
     Agpl.Smart_Access;
with Agpl.Types;

with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics;

package Sancta.Types is

   pragma Preelaborate;

   type Agent_Kinds is (Robot, Gui, Planner, Unknown);

   type Agent_Name_Array is array (Character range 'a' .. 'z') of Node_Id;

   function Agent_Names return Agent_Name_Array;
   --  Returns a set of predefined names.

   type Real is new Long_Float;
   --  Generic floating point for use in distances, angles, coordinates.

   --  All coordinates in meters, all angles in radians.

   type Angle is new Real;

   Angle_Pi : constant Types.Angle := Types.Angle (Ada.Numerics.Pi);

   type Counter32 is mod 2**32;

   function To_Angle (A : in Float) return Angle;
   function To_Angle (A : in Real)  return Angle;
   --  Performs normalization too (around zero).

   function Normalize_Positive (Angle : in Real) return Real;
   --  Moves the angle to 0 .. 2 Pi.

   function Normalize_Zero (Angle : in Real) return Real;
   --  Moves the angle to -Pi .. Pi.

   function "+" (L, R : in Angle) return Angle;
   function "-" (L, R : in Angle) return Angle;
   --  These ensure the angles are always in -Pi .. +Pi range.

   type Point is record
      X, Y : Real;
   end record;

   type Pose is record
      X, Y : Real;
      A    : Angle;
   end record;

   function To_Point (P : Pose) return Point;
   function "+"      (P : Pose) return Point renames To_Point;
   pragma Inline (To_Point);
   pragma Inline ("+");

   function Image (P : Pose; Decimals : Natural := 2) return String;

   type Local_Pose  is new Pose;
   --  Differentiated type to avoid mixins
   --  These are poses in some local reference frame

   type Global_Pose is new Pose;
   --  These are poses in some global reference frame

   package Protected_Pose is new Agpl.Protected_Value (Pose);

   Null_Pose : constant Pose := (0.0, 0.0, 0.0);
   Origin    :          Pose renames Null_Pose;

   function "+" (L, R : in Pose) return Pose; pragma Inline ("+");
   function "-" (L, R : in Pose) return Pose; pragma Inline ("-");
   --  Component by component addition.
   function "/" (L : Pose; R : Real) return Pose; pragma Inline ("/");

   type Pose_Array is array (Integer range <>) of Pose;

   package Pose_Vector is new Agpl.Containers.Naked_Vectors (Pose);

   type Range_Reading is record
      A        : Types.Angle; -- Angle
      D        : Types.Real; -- Distance
   end record;
   --  Used for range-based sensors: RF, IR, Laser.

   type Range_Scan is array (Positive range <>) of Range_Reading;
   --  A complete scan of a range sensor.

   type Cloud_Scan is array (Positive range <>) of Pose;
   --  Range scan converted to some coordinate frame

   type Full_Scan (Last : Natural) is record
      Ranges : Range_Scan (1 .. Last);
      Poses  : Cloud_Scan (1 .. Last);
   end record;

   type Posed_Range_Scan (Last : Natural) is record
      Pose : Types.Pose;
      Scan : Range_Scan (1 .. Last);
   end record;

   package Posed_Range_Scan_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, Posed_Range_Scan);

   type Full_Scan_Access is access Full_Scan;

   package Smart_Full_Scan_Access is new Agpl.Smart_Access
     (Full_Scan, Full_Scan_Access, "Full_Scan");

   subtype Rgb_Component is Agpl.Types.Unsigned_8;

   type Percent is new Float range 0.0 .. 100.0;

   subtype Colors is Agpl.Types.Rgb_Triplet;

   type User_Data is tagged limited null record;
   --  For use in graphical callbacks.

   type User_Data_Access is access all User_Data'Class;

   --  types for discrete coordinates so not to mix them...
   type X_Coordinate is new Integer;
   subtype Abscissa  is X_Coordinate;
   subtype X_Coord   is X_Coordinate;
   subtype Columns   is X_Coordinate;

   type Y_Coordinate is new Integer;
   subtype Ordinate  is Y_Coordinate;
   subtype Y_Coord   is Y_Coordinate;
   subtype Rows      is Y_Coordinate;

   --  Likewise for the real domain...
   type X_Real is new Real;
   type Y_Real is new Real;

   --------------------------------------------------------------------------

   function "+" (X : in Types.Real) return Types.Angle renames To_Angle;

   function "+" (X : in Types.Angle) return Types.Real;

   function "+" (X : in Types.Real) return Float;

   function "+" (X : in Float) return Types.Real;

   function "+" (X : in Agpl.Cv.Point2D) return Types.Pose;

   function "+" (X : in Types.Pose) return Agpl.Cv.Point2D;

   function "+" (X : in X_Coordinate) return Integer;

   function "+" (X : in Y_Coordinate) return Integer;

private

   pragma Inline ("+", To_Angle, Normalize_Zero, Normalize_Positive);

end Sancta.Types;
