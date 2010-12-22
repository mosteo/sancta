 

--  A gap is a segment.

with Agpl.Cv;
--  with Agpl.Containers.Naked_Vectors;
with Agpl; use Agpl;

package Sancta.Gap is

   pragma Preelaborate;

   type Kinds is (Occlusive, Out_of_range);
   --  Occlusive gaps are formed because some vertex prevents vision.
   --  Out of range gaps occur when the range sensor don't reach anything.

   type Object is private;

   type Object_Array is array (Positive range <>) of Object;

   function Create (P1, P2 : Cv.Point2D;
                    Kind   : Kinds := Occlusive) return Object;
   --  Create a Gap which starts at P1 and ends at P2

   function Get_Line (This : Object) return Cv.Line2D;

   function Get_Start (This : Object) return Cv.Point2D;

   function Get_End   (This : Object) return Cv.Point2D;

   function Get_Kind  (This : Object) return Kinds;

   --  Debug

   procedure Dump (This : Object);
   --  Dump description to stdout.

private

   type Object is record
      P1, P2 : Cv.Point2D;
      Line   : Cv.Line2D;
      Kind   : Kinds;
   end record;

   pragma Inline (Create, Get_Line, Get_Start, Get_End, Get_Kind);

end Sancta.Gap;
