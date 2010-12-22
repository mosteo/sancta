 

with Ada.Containers.Indefinite_Ordered_Maps;

package Sancta.Tasks.Maps is
new Ada.Containers.Indefinite_Ordered_Maps
  (Task_Id, Tasks.Object'Class, "<", Same_Id);

pragma Preelaborate (Sancta.Tasks.Maps);
