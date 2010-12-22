 

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Sancta.Controller_Observer.Lists is
   new Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

pragma Preelaborate (Sancta.Controller_Observer.Lists);
