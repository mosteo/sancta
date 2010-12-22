 

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Cost_Cache;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Plan;
with Sancta.Tasks.Containers;

package Sancta.Assigner is

   pragma Preelaborate;

   type Object is abstract tagged null record;

--     function Assign
--       (This   : in Object;
--        Agents : in Agent.Containers.Lists.List;
--        Tasks  : in Sancta.Tasks.Containers.Lists.List)
--        return      Assignment.Object is abstract;
   --  Takes a bunch of agents and tasks and says who must perform each task.
--  DEPRECATED. To avoid multiple calculation of costs.

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return      Assignment.Object is abstract;

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Containers.Lists.List;
      Plans          : in     Sancta.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Sancta.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object);
   --  The plan maintains the methods and all that.
   --  No need to pass fake starting tasks, since this will be taken care of
   --  in the inside.

   --  DEBUG

   procedure Assign_Best_Plan
     (The_Assigner   : in     Object'Class;
      Agents         : in     Agent.Containers.Lists.List;
      Plans          : in     Sancta.Plan.Object; -- OR of possible plans.
      Criterion      : in     Assignment_Criteria;
      Plan           :    out Sancta.Plan.Object; -- Selected plan with best cost.
      The_Assignment :    out Assignment.Object;
      Enumerate      : access procedure (A : in Assignment.Object));
   --  As previous, but will call @Enumerate@ with each considered assignment

private

   package Ac renames Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;

end Sancta.Assigner;
