--  Global options
pragma Detect_Blocking;

private with Ada.Strings.Bounded;

with Agpl.Conversions;

pragma Warnings (Off);
with Agpl.Trace; use Agpl.Trace;
pragma Warnings (On);

package Sancta is

   pragma Preelaborate;

   Allow_Uppercase_Node_Ids : Boolean := False;

   type Costs is delta 0.01 digits 18;
   --  This *must* be a fixed point type, otherwise incremental cost computation
   --  in annealing will fail.

   function To_String is new Agpl.Conversions.Decimal_To_Str (Costs);

   pragma Warnings (Off);
   use type Costs;
   pragma Warnings (On);

   Infinite : constant Costs := Costs'Last;

   type Node_Id is private;

   function "<" (L, R : in Node_Id) return Boolean; pragma Inline ("<");

   function Image (Id : in Node_Id) return String;  pragma Inline (Image);
   function "-"   (Id : in Node_Id) return String renames Image;
   function Value (Id : in String)  return Node_Id; pragma Inline (Value);
   function "+"   (Id : in String)  return Node_Id renames Value;

   function No_Node return Node_Id;   pragma Inline (No_Node);
   function All_Nodes return Node_Id; pragma Inline (All_Nodes);

   type Unordered_Node_Pair is private;
   function Value (Id_1, Id_2 : String) return Unordered_Node_Pair;
   function Value (Id_1, Id_2 : Node_Id) return Unordered_Node_Pair;
   function "<" (L, R : Unordered_Node_Pair) return Boolean;

private

   package Ids is new Ada.Strings.Bounded.Generic_Bounded_Length (20);

   type Node_Id is new Ids.Bounded_String;

   type Unordered_Node_Pair is record
      L, R : Node_Id;
   end record;

end Sancta;
