

with Sancta;
with Sancta.Agent;
with Sancta.Agent.Containers;
with Sancta.Agent.Handle;
with Sancta.Assignment;
with Sancta.Cost_Cache;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Tasks;
with Sancta.Tasks.Containers;

--  Insertion of tasks into agents

package Sancta.Tasks.Insertions is

   --  pragma Preelaborate;

   package Ac renames Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;

   type Insertion_Procedures is access
     procedure (A          : in     Agent.Object'Class;
                T          : in     Sancta.Tasks.Object'Class;
                New_Agent  :    out Agent.Handle.Object;
                Cost_Delta :    out Sancta.Costs;
                Cost_Total :    out Sancta.Costs;
                Success    :    out Boolean);

   procedure Before_Id (List    : in out Sancta.Tasks.Containers.Lists.List;
                        Job     : in     Sancta.Tasks.Object'Class;
                        Id      : in     Sancta.Tasks.Task_Id;
                        Is_Last : in     Boolean := False);
   --  Insert in the given list, before task Id
   --  or else, if Is_Last, append at end.
   --  If Id is not found then raise Program_Error

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Sancta.Tasks.Object'Class;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Sancta.Costs;
                     Cost_Total :    out Sancta.Costs;
                     Success    :    out Boolean);
   --  Tests the best place where to insert task T for the agent.
   --  The task list is not reordered, only each place is tried.
   --  Returns a copy of @A@ with the task inserted in New_Agent.
   --  Returns the increase in cost in @Cost_Delta@
   --  Returns total cost for agent agenda in @Cost_Total@
   --  New_Agent will be of same class than A
   --  Success will be false if the agent can't insert T at any place.

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Sancta.Tasks.Object'Class;
                     C          : in     Cost_Cache.Object'Class;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Sancta.Costs;
                     Cost_Total :    out Sancta.Costs;
                     Success    :    out Boolean);
   --  Tests the best place where to insert task T for the agent.
   --  The task list is not reordered, only each place is tried.
   --  Costs aren't given by the agent but taken from the cost_matrix
   --  Returns a copy of @A@ with the task inserted in New_Agent.
   --  Returns the increase in cost in @Cost_Delta@
   --  Returns total cost for agent agenda in @Cost_Total@
   --  New_Agent will be of same class than A
   --  Success will be false if the agent can't insert T at any place.

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Sancta.Tasks.Object'Class;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Sancta.Costs;
                     Cost_Total :    out Sancta.Costs;
                     Success    :    out Boolean);
   --  As previous, but with Not_Before you can force an amount of tasks to
   --  not be considered

   procedure Greedy (Ass       : in     Assignment.Object;
                     T         : in     Sancta.Tasks.Object'Class;
                     Costs     : in     Cost_Cache.Object'Class;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Success   :    out Boolean;
                     Random    : in     Boolean := False);
   --  Insert a task in the best place of the best agent of an assignment
   --  The results are given in New_Ass, with Success true.
   --  If random, a random agent is chosen on tie. If not, the first one wins

   procedure Greedy (Ass       : in     Assignment.Object;
                     Tasks     : in     Sancta.Tasks.Containers.Lists.List;
                     Costs     : in     Cost_Cache.Object'Class;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Inserted  :    out Sancta.Tasks.Task_Id;
                     Random    : in     Boolean := False);
   --  Insert the best task of the list in the best agent.
   --  Just *one* task is inserted.
   --  Inserted can be No_Task if failure.
   --  If random, a random agent is chosen on tie. If not, the first one wins

   procedure Greedy_Tail (Agent      : in Sancta.Agent.Object'Class;
                          Tasks      : in Sancta.Tasks.Containers.Lists.List;
                          Costs      : in     Cost_Cache.Object'Class;
                          New_Agent  :    out Sancta.Agent.Handle.Object;
                          Inserted   :    out Sancta.Tasks.Task_Id;
                          Cost_Total :    out Sancta.Costs;
                          Cost_Delta :    out Sancta.Costs);
   --  Best task to the tail of the agent

   procedure Greedy_Tail (Ass        : in Assignment.Object;
                          T          : in Sancta.Tasks.Object'Class;
                          Costs      : in Cost_Cache.Object'Class;
                          Criterion  : in     Assignment_Criteria;
                          New_Ass    :    out Assignment.Object;
                          New_Agent  :    out Sancta.Agent.Handle.Object;
                          Cost_Total :    out Sancta.Costs;
                          Cost_Delta :    out Sancta.Costs;
                          Success    :    out Boolean);
   --  Append the task in the best agent.

   procedure Greedy_Tail (Ass       : in     Assignment.Object;
                          Tasks     : in     Sancta.Tasks.Containers.Lists.List;
                          Costs     : in     Cost_Cache.Object'Class;
                          Criterion : in     Assignment_Criteria;
                          New_Ass   :    out Assignment.Object;
                          New_Agent :    out Sancta.Agent.Handle.Object;
                          Inserted  :    out Sancta.Tasks.Task_Id;
                          Cost_Total :    out Sancta.Costs;
                          Cost_Delta :    out Sancta.Costs);
   --  Insert the best task in the best agent.
   --  Just *one* task is inserted.
   --  The task is only tried at end of agent plans.
   --  The agent gaining the task is given (with the task).

   procedure Idle_Tail (Ass        : in     Assignment.Object;
                        Tasks      : in     Sancta.Tasks.Containers.Lists.List;
                        Costs      : in     Cost_Cache.Object'Class;
                        New_Ass    :    out Assignment.Object;
                        Inserted   :    out Sancta.Tasks.Task_Id;
                        Cost_Total :    out Sancta.Costs;
                        Cost_Delta :    out Sancta.Costs);
   --  Append in the less busy agent its best task from Tasks

   function Greedy (Agents    : Ac.Lists.List;
                    Tasks     : Tc.Lists.List;
                    Costs     : Cost_Cache.Object'Class;
                    Criterion : Assignment_Criteria := Criterion_Minmax;
                    Random    : Boolean                := False)
                    return   Sancta.Assignment.Object;
   --  just assign the best one to the best agent and give such an assignment.

   function Closest_Agent (Agents    : Ac.Lists.List;
                           Tasks     : Tc.Lists.List;
                           Costs     : Cost_Cache.Object'Class;
                           Random    : Boolean := False)
                           return Sancta.Agent.Object'Class;
   --  Identify closest agent to one of these tasks.

   function Closest_Task (Agents    : Ac.Lists.List;
                          Tasks     : Tc.Lists.List;
                          Costs     : Cost_Cache.Object'Class;
                          Random    : Boolean := False)
                          return Sancta.Tasks.Object'Class;
   --  Identify the closest task to any of the agents.
   --  if Agents or Tasks is empty or all costs infinite, constraint_error.

   function Closest_Task (Agents    : Ac.Lists.List;
                          Tasks     : Tc.Lists.List;
                          Costs     : Cost_Cache.Object'Class;
                          Random    : Boolean := False)
                          return Sancta.Tasks.Task_Id;

   --  LIST MANIPULATION

   function Contains (T  : Sancta.Tasks.Containers.Lists.List;
                      Id : Sancta.Tasks.Task_Id)
                      return Boolean;

   procedure Remove (T  : in out Sancta.Tasks.Containers.Lists.List;
                     Id :        Sancta.Tasks.Task_Id;
                     Fail_If_Missing : Boolean := True);

   procedure Remove (T : in out Sancta.Tasks.Containers.Lists.List;
                     X :        Sancta.Tasks.Containers.Lists.List;
                     Fail_If_Missing : Boolean := True);

end Sancta.Tasks.Insertions;
