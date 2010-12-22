 

with Sancta.Cost_Cache;

package Sancta.Assigner.Greedy_Fifo_Tail is

   --  Greedy heuristic that at each step will select the less occupied agent
   --  and select for him his best task (iterated assignment).
   --  The new task for an agent will be tried just at end of plan.
   --  This mimics an agent that when finishing a task just asks for the next
   --  one he is best suited for.

   --  O (T * A * T) ~ O (n^3)

--  pragma Preelaborate;

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;

end Sancta.Assigner.Greedy_Fifo_Tail;
