 

with Sancta.Types;

with Sancta.Agent.Containers;
with Sancta.Assigner;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Sancta.Tasks.Containers;

package Sancta.Assigners.Greedy is

--   pragma Elaborate_Body;

   type Object is new Sancta.Assigner.Object with record
      Umbral : Types.Real := 0.1;
   end record;
   --  This assigner is identical to the Sancta.Hungry except for the following:
   --  When a new task is selected, it is checked if its closer than Umbral
   --  to some agent. In that case, it is assigned to said agent instead of
   --  the most unused.
   --  This is a simple hack for our first experiments.

   function Assign
     (This   : in Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Matrix : in Sancta.Cost_Matrix.Object)
      return      Sancta.Assignment.Object;
   --  Hungry heuristic that will attempt to get the less-cost agent for each
   --  task sequence.

end Sancta.Assigners.Greedy;
