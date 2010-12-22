 

with Sancta.Cost_Cache;

package Sancta.Assigner.Greedy_Best_Pair_Tail is

   --  Greedy heuristic that at each step will select the pair agent-task which
   --  best fits the criterion.
   --  The new task for an agent will be tried just at end of plan.

   --  O (T * A * T) ~ O (n^3)

--  pragma Preelaborate;

   type Object is new Assigner.Object with record
      Criterion : Assignment_Criteria := Criterion_Time_Critical;
   end record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;

end Sancta.Assigner.Greedy_Best_Pair_Tail;
