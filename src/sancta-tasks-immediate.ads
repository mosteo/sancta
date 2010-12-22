 

--  Tasks of the network.

--  Primitive specialization.

package Sancta.Tasks.Immediate is

   pragma Preelaborate;

   type Object is interface;
   type Object_Access is access Object'Class;

   --  There's nothing to implement. Inmediate tasks are those that must be
   --  at first point of the task list

end Sancta.Tasks.Immediate;
