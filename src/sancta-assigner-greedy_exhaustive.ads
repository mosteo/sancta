 

with Sancta.Cost_Cache;

package Sancta.Assigner.Greedy_Exhaustive is

   --  Greedy heuristic that at each step will select the pair agent-task which
   --  best fits the criterion.
   --  The new task for an agent will be tried at all points of its plan.

   --  O (T * A * T * T) ~ O (n^4)

   --  pragma Preelaborate;

   type Object is new Assigner.Object with record
      Randomize : Boolean             := False;
      Criterion : Assignment_Criteria := Criterion_Time_Critical;
   end record;
   --  Random is true, when two agents are tied in a step, the winner is chosen
   --  at random.

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;

   function Assign
     (Criterion : Assignment_Criteria;
      Randomize : Boolean;
      Agents    : Agent.Containers.Lists.List;
      Tasks     : Sancta.Tasks.Containers.Lists.List;
      Costs     : Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;
   --  Convenience

end Sancta.Assigner.Greedy_Exhaustive;
