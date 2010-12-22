 

with Sancta.Cost_Cache;

--  Greedy heuristic that will at each step use the best pair agent-task
--  with task adding less cost.
--  Not exhaustive: tasks are always added to the end of an agent tasks list

--  O (T * A * T) ~ O (n^3)

package Sancta.Assigner.Greedy_Minsum_Best_Pair_Tail is

   pragma Preelaborate;

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;

end Sancta.Assigner.Greedy_Minsum_Best_Pair_Tail;
