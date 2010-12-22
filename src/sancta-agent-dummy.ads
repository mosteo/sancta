 

--  This agent doesn't know how to do anything, but can be used as a placeholder
--  is Assignation objects :/

package Sancta.Agent.Dummy is

   pragma Preelaborate;

   type Object is new Agent.Object with private;

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean);
   --  Will do nothing; Done will be always false.

private

   type Object is new Agent.Object with null record;

end Sancta.Agent.Dummy;
