 

--  Auxiliary type to index by agent x task

with Sancta.Agent;
with Sancta.Tasks;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers;

package Sancta.Agent_Task_Key is

   pragma Preelaborate;

   type Object (<>) is private;

   function Get (A : in Sancta.Agent.Object'Class;
                 T : in Sancta.Tasks.Object'Class) return Object;

   function Hash (This : in Object) return Ada.Containers.Hash_Type;

private

   type Object is record
      Agent   : Ustring;
      Tid     : Sancta.Tasks.Task_Id;
   end record;


end Sancta.Agent_Task_Key;
