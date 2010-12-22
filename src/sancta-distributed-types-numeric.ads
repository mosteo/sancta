with Sancta.Distributed.Types;
pragma Elaborate_All (Sancta.Distributed.Types);

package Sancta.Distributed.Types.Numeric is

   --  Pragma preelaborate;

   package Dfloats is new Dreals (Float);

   subtype Dfloat is DFloats.Dreal;
   function Build (This : in Float)  return Dfloat renames Dfloats.Build;
   function Value (This : in Dfloat) return Float  renames Dfloats.Value;
   function Image (This : in Dfloat) return String renames Dfloats.Image;

end Sancta.Distributed.Types.Numeric;
