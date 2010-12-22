with Sancta.Types;

with Sancta;
with Sancta.Tasks.Containers;

package Sancta.Debug is

   pragma Preelaborate;

   procedure Print (This : in Sancta.Tasks.Containers.Lists.List);
   --  Listing of tasks in a list.

   generic
      type Num is digits <>;
   function To_String_X (This : in Num) return String;

   function To_String (This : in Sancta.Costs) return String;
   function "+" (This : in Sancta.Costs) return String renames To_String;

   function To_String (This : in Types.Angle) return String;
   --  Human readable with 5 decimal digits.

   function To_String (This : in Types.Real) return String;
   --  Human readable with 3 decimal digits.

   function To_String (This : in Types.Pose) return String;
   --  Human readable.

end Sancta.Debug;
