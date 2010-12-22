 

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Sancta.Method.Lists is
new Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

pragma Preelaborate (Sancta.Method.Lists);
