 

with Ada.Containers.Indefinite_Vectors;

package Sancta.Method.Vectors is
new Ada.Containers.Indefinite_Vectors (Positive, Object'Class);

pragma Preelaborate (Sancta.Method.Vectors);
