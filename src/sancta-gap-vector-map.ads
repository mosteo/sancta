 

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Sancta.Gap.Vector.Map is
new Ada.Containers.Indefinite_Hashed_Maps
  (String,
   Vector.Object,
   Ada.Strings.Hash,
   "=");

pragma Preelaborate (Sancta.Gap.Vector.Map);
