 

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Sancta.Assigner.Lists is new
Ada.Containers.Indefinite_Doubly_Linked_Lists (Assigner.Object'Class);

pragma Preelaborate (Sancta.Assigner.Lists);
