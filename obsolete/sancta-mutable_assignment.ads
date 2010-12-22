 

with Agpl.Cr.Agent.Containers;
with Agpl.Cr.Assignment;
with Agpl.Htn.Plan;
with Agpl.Htn.Plan_Node;
with Agpl.Htn.Tasks;
with Agpl.Htn.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Smart_Access;
use Agpl;

with Ada.Containers.Vectors;

--  An adhoc assignment with the required operations for annealing
--  and with tasks equivalent one to one

package Sancta.Mutable_Assignment is

   pragma Elaborate_Body;

   --   type Object is new Agpl.Cr.Assignment.Object with private;
   type Object is tagged private;

   function Completed (This : in Object) return Boolean;
   --  Say if planning has been completed (all tasks are frozen).

   function Evaluate_Minimax
     (This : in Object) return Optimization.Cost;

   function Evaluate_Totalsum
     (This : in Object) return Optimization.Cost;

   function Invalid_Assignment return Object;

   function Is_Valid (This : in Object) return Boolean;

   function Last_Mutation (This : in Object) return String;

   procedure Mark_Elapsed (This    : in out Object;
                           Elapsed : in     Duration;
                           Changes :    out Boolean);
   --  This is used to froze tasks already executed or being executed.
   --  Changes will signal if there's some change in frozen tasks.

   function Mutate (This : in Object) return Object;

   function Normalize (Old_Cost,
                       New_Cost : in Optimization.Cost;
                       Temp     : in Optimization.Annealing.Temperature)
                       return        Optimization.Annealing.Acceptability;

   function To_Assignment (This : in Object)
                           return    Agpl.Cr.Assignment.Object;
   --  Get the embedded assignment (Costly operation!)

   function To_Mutable (Agents : in Agpl.Cr.Agent.Containers.Lists.List;
                        Plan   : in Agpl.Htn.Plan.Object) return Object;
   --  Creation from agents and a *NON-EXPANDED* plan
   --  No initial assignation, use Update for that.

   procedure Update (This       : in out Object;
                     Assignment : in     Agpl.Cr.Assignment.Object);
   --  Take an already prepared Object and build the references again from
   --  the assignment solution.

   procedure Print_Summary (This : in Object);
   --  Debug dump to stdout

private

   type Mutations is (None,
                      Heuristic_All,
                      Heuristic_One,
                      Flip_In_Place,
                      Flip_First_In_Place, -- Includes Heuristic_One
                      Guided_Flip_In_Place,
                      Guided_Flip_First_In_Place,
                      Move,
                      Flip_Move,
                      Guided_Move,
                      Guided_Flip_Move);

   type Context_Type is record
      Agents   : Agpl.Cr.Agent.Containers.Lists.List;

      Plan     : Agpl.Htn.Plan.Object;
      --  The inflated plan (all nodes expanded)
   end record;

   type Context_Access is access Context_Type;

   package Smart_Context is new Agpl.Smart_Access (Context_Type, Context_Access);
   --  This is used for quick copy of mutations, all of them referencing
   --  to the same data.

   package Subplan_Vectors is new
     Ada.Containers.Vectors (Positive,
                             Agpl.Htn.Plan.Subplan,
                             Agpl.Htn.Plan_Node."=");
   --  This will hold a vector of pointers to primitive task nodes.
   --  In essence these are the tasks to be performed by an agent.


   package Assignation_Vectors is new
     Ada.Containers.Vectors (Positive,
                             Subplan_Vectors.Vector,
                             Subplan_Vectors."=");
   --  This is a vector where each element is an assignation of Subplan_Vectors.
   --  The index refers to an agent. To get the agent with this number, we'll
   --  use the position in the context object.

   package Agent_Locators is new
     Ada.Containers.Vectors (Positive,
                             Agpl.Cr.Agent.Containers.Lists.Cursor,
                             Agpl.Cr.Agent.Containers.Lists."=");
   --  Pointers to the list of agents. This allows quick obtention of some agent.

   package Agent_Unfrozen is new
     Ada.Containers.Vectors (Positive, Positive);
   --  The value is the first task that is unfrozen for an agent

   type Object is tagged record
      Context       : Smart_Context.Object;

      Agent_Locator : Agent_Locators.Vector;

      Ass           : Assignation_Vectors.Vector;

      Unfrozen      : Agent_Unfrozen.Vector;

      Last_Mutation : Mutations := None;
      Agent_Ini,
      Agent_Fin     : Natural   := 0;
      Task_Ini,
      Task_Fin      : Natural   := 0;

      Valid         : Boolean   := True; -- To mark an invalid assignment
   end record;

   --  This type has to be lightweight since there's quite copying of it around.

   function Evaluate_Agent (This  : in Object;
                            Agent : in Positive -- Which agent
                            ) return   Agpl.Optimization.Cost;

   procedure Flip_Given_Task (Pos : in Subplan_Vectors.Cursor);
   --  Will select a random sibling of given node and exchange with it.
   --  If no siblings, himself.
   --  Use this for in-place flips.

   function Flip_Given_Task (Job : in Htn.Plan.Subplan) return Htn.Plan.Subplan;
   --  Will select a random sibling of given node and exchange with it.
   --  If no siblings, himself.
   --  Use this for removed flips.

   procedure Flip_Random_Task (This : in out Object);
   --  Will select a random task and flip it.

   function Get_Agent_Tasks (This  : in Object;
                             Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List;
   --  Extract a list with the tasks being used

   function Get_Agent_Unfrozen_Tasks
     (This  : in Object;
      Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List;
   --  Extract a list with the tasks being used but still unfrozen

   function Get_Agents_With_Frozen_Tasks (This : in Object)
                                          return    Agpl.Cr.Agent.Containers.Lists.List;
   --  Get a list with the agents and its already assigned tasks

   function Get_Assigned_Tasks (This : in Object) return Agpl.Htn.Tasks.Containers.Lists.List;
   --  Get a list with all the tasks being assigned to some agent

   function Get_Assigned_Unfrozen_Tasks (This : in Object)
                                         return    Agpl.Htn.Tasks.Containers.Lists.List;
   --  A list with all tasks assigned but still unfrozen.

   function Get_Frozen_Tasks (This  : in Object;
                              Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List;
   --  Get a list with frozen task of an agent

   function Get_Most_Costly_Agent (This : in Object) return Positive;
   --  Returns an index to the assignation vector

   procedure Heuristic_One (This : in out Object; Which : in Natural := 0);
   --  Optimize single agent tasks, greedily considering all OR alternatives.
   --  Use which not to use a random one

   procedure Insert_At_Random (This : in out Object;
                               Job  : in     Htn.Plan.Subplan);
   --  Insert a given task in a random agent and plan point.

   procedure Select_Random_Task (This   : in out Object;
                                 Remove : in     Boolean;
                                 Job    :    out Htn.Plan.Subplan;
                                 Pos    :    out Subplan_Vectors.Cursor);
   --  Select a random task from any agent and optionally
   --  remove it from its list.
   --  The cursor returned is a pointer from the vector in This.Ass
   --  The cursor can be invalid (failed to choose task)
   --  Note that when remove is true, Pos is invalid, but Job is valid

   procedure Select_Random_Task_From_Agent (This   : in out Object;
                                            Agent  : in     Positive;
                                            Remove : in     Boolean;
                                            Job    :    out Htn.Plan.Subplan;
                                            Pos    :    out Subplan_Vectors.Cursor);
   --  Select a random task from a given agent by index.

   procedure Select_And_Remove_Random_Task
     (This : in out Object;
      Job  :    out Htn.Plan.Subplan);
   --  As previous but discarding the position and always removing the task

end Sancta.Mutable_Assignment;
