with Sancta.Agent;

package Sancta.Tasks.Interfaces is

   pragma Preelaborate;

   type Completable is interface;
   --  Generic to know if a task has been completed

   function Completed (This  : Completable;
                       Agent : Sancta.Agent.Object'Class) return Boolean
                       is abstract;

end Sancta.Tasks.Interfaces;
