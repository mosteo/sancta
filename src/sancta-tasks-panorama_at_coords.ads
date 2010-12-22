with Sancta.Types;

with Sancta.Tasks.Compound;

package Sancta.Tasks.Panorama_At_Coords is

   pragma Preelaborate;

   type Object is new
     Sancta.Tasks.Compound.Object with
      record
         Coords : Types.Point;
      end record;
   --  Values in meters.

end Sancta.Tasks.Panorama_At_Coords;
