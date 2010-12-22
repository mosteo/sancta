 

--  This agent doesn't know how to do anything, but can be used as a placeholder
--  is Assignation objects :/

package body Sancta.Agent.Dummy is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean)
   is
      pragma Unreferenced (This, The_Task, Plan);
   begin
      Done := False;
   end Execute;

end Sancta.Agent.Dummy;
