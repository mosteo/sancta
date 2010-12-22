

--  An assignment says who must perform each task in a plan.

with Sancta.Agent.Containers;
with Sancta.Cost_Cache;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Plan;
with Sancta.Tasks;
with Sancta.Tasks.Containers;

with Agpl.Containers.String_Sets;

package Sancta.Assignment is

   pragma Preelaborate;

   type Object is tagged private;

   function Empty_Object return Object'Class;
   --  function Empty_Object return Object;
   --  This would require overriding in all descendents

   function Create (Agent : Sancta.Agent.Object'Class) return Object;
   function Create (Agents : Sancta.Agent.Containers.Lists.List)
                    return   Object;
   pragma Inline (Create);

   function Is_Empty (This : Object) return Boolean;
   --  Says if there are tasks
   --  If there are agents without tasks, it will be considered empty aswell

   procedure Add
     (This     : in out Object;
      Agent    : in     Sancta.Agent.Object'Class;
      The_Task : in     Sancta.Tasks.Object'Class);
   --  Add a task to some agent

   procedure Add
     (This      : in out Object;
      Agent     : in     Sancta.Agent.Object'Class;
      The_Tasks : in     Sancta.Tasks.Containers.Lists.List);

   procedure Prepend
     (This     : in out Object;
      Agent    : in     Sancta.Agent.Object'Class;
      The_Task : in     Sancta.Tasks.Object'Class);
   --  Add a task to the head of the list

   procedure Set_Task (This  : in out Object;
                       Agent :        String;
                       Job   :        Sancta.Tasks.Object'Class);

   procedure Set_Tasks (This  : in out Object;
                        Agent :        String;
                        Jobs  :        Sancta.Tasks.Containers.Lists.List);

   procedure Copy_Tasks (Dst : in out Object;
                         Src :        Object);

   procedure Merge_Missing_Robots (Dst        : in out Object;
                                   Src        :        Object;
                                   With_Tasks :        Boolean := True);

   procedure Clear (This : in out Object);
   --  Remove all robots

   procedure Clear_Tasks (This : in out Object);
   --  Remove tasks from robots

   procedure Clear_Tasks (This  : in out Object;
                          Agent :        String);
   --  Remove tasks of only one robot

   function Contains (This : in Object; Name : in String) return Boolean;
   --  Says if the named agent is in the assignment.

   function Contains (This : in Object; Id : in Sancta.Tasks.Task_Id)
                      return Boolean;
   --  Says if some of the agents has this task in its plan.

   procedure Fill_Owners (This : in Object; Plan : in out Sancta.Plan.Object);
   --  Fill the owners of tasks in a plan with this assignment.

   function Get_Agent (This : in Object; Name : in String)
                       return Agent.Object'Class;
   --  Gets an agent copy with all its tasks.

   function Get_Agent (This : Object; Id : Sancta.Tasks.Task_Id)
                       return Agent.Object'Class;
   --  Get the first owner of a given task

   function Get_Agents (This : Object; Id : Sancta.Tasks.Task_Id)
                        return Agent.Containers.Lists.List;
   --  Return all agents owning a task.

   procedure Remove_Agent (This : in out Object; Name : String);
   procedure Remove_Agents (This  : in out Object;
                            Names :        Agent.Containers.Lists.List);

   function Get_Less_Costly_Agent (This : in Object) return Agent.Object'Class;
   --  Get a copy of the agent with less cost (including idles!)

   function Get_Less_Costly_Non_Idle_Agent (This : in Object)
                                            return Agent.Object'Class;

   function Get_Most_Costly_Agent (This : in Object) return Agent.Object'Class;
   --  Get a copy of the agent with longest task list

   function Get_Idle_Agents (This : Object)
                             return Sancta.Agent.Containers.Lists.List;

   function Get_Non_Idle_Agents (This : Object)
                                 return Sancta.Agent.Containers.Lists.List;

   procedure Set_Agent (This : in out Object; Agent : in Sancta.Agent.Object'Class);
   --  Add or replace an agent and its tasks

   procedure Set_Agents (This   : in out Object;
                         Agents :        Sancta.Agent.Containers.Lists.List);
   --  Same for an agent lists. Doesn't clear previous agents in there.

   procedure Fill_Missing_Agents (This   : in out Object;
                                  Agents :        Sancta.Agent.Containers.Lists.List);
   --  Merge but keeping existing tasks

   function Get_Agents (This : in Object)
                        return Agent.Containers.Lists.List;
   --  Get all agents with its tasks.

   function Get_Agents (This  : Object;
                        Which : Agpl.Containers.String_Sets.Set)
                        return Object'Class;

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Lists.List;

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Vectors.Vector;

   function Get_All_Tasks (This : in Object) return Sancta.Tasks.Containers.Lists.List;
   --  Return all tasks in the assignment, regardless of owner agent.

   function Get_All_First_Tasks (This : in Object) return Sancta.Tasks.Containers.Lists.List;
   --  Return all first tasks of all agents

   function Get_Tasks (This  : in Object;
                       Agent : in Sancta.Agent.Object'Class)
                       return Sancta.Tasks.Containers.Lists.List;
   --  Says the tasks assigned to a particular agent.

   function Get_Tasks (This  : in Object;
                       Agent : in String)
                       return Sancta.Tasks.Containers.Lists.List;
   --  Says the tasks assigned to a particular agent.

   procedure Process (This : in out Object;
                      What : access procedure (A : in out Agent.Object'Class));
   --  Call What for all agents in this assignment

   function Get_Max_Min_Cost (This : in Object) return Costs;
   function Get_Max_Min_Cost (This : in Object;
                              C    : in Cost_Cache.Object'Class) return Costs;
   --  Says the worst of all the agent total costs.

   function Get_Cummulative_Cost (This : in Object) return Costs;
   function Get_Cummulative_Cost (This : in Object;
                                  C    : in Cost_Cache.Object'Class) return Costs;
   --  Says the sum of all agent costs.

   function Get_Average_Cost (This : Object;
                              Div  : Boolean := True) return Costs;
   function Get_Average_Cost (This : Object;
                              C    : Cost_Cache.Object'Class;
                              Div  : Boolean := True) return Costs;
   --  Average cost per task (average wait until a task is completed, for all tasks)
   --  if not Divide, the total amount is not divided by the number of tasks.
   --  This is useful to reuse this function when bidding, since there we
   --  have to divide by the total tasks and not the currently allocated.

   function Get_Cost (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs;
   function Get_Cost (This      : in Object;
                      C         : in Cost_Cache.Object'Class;
                      Criterion : in Assignment_Criteria) return Costs;
   --  Uses one of the two previous according to the Criterion

   function Get_Cost_Until_Task (This      : in Object;
                                 Job       : in Sancta.Tasks.Task_Id;
                                 Criterion : in Assignment_Criteria)
                                 return    Sancta.Costs;
   --  Says the cost incurred until finishing

   function Invalid_Assignment return Object'Class;
   --  Returns an invalid assignment.

   function Is_Valid (This : in Object) return Boolean;

   procedure Set_Valid (This : in out Object; Valid : in Boolean := True);

   function Freeze_Plan (This : in Object;
                         P    : in Sancta.Plan.Object)
                         return    Sancta.Plan.Object;
   --  This will take a plan that contains a superset of the tasks in the
   --  assignment. If the plan contains OR nodes, these will be replaced with
   --  the branches used by the assignment.
   --  If some incompability is detected (tasks in This but not in P, or
   --  sibling tasks used in This), an exception will be raised.
   --  Note that if P > This, the plan can be just partially frozen.

   --  DEBUG

   procedure Print_Assignment (This : in Object);
   procedure Print_Summary (This : in Object);

   function Dummy_Key (This : Object) return String;

private

   type Object is tagged record
      Ok     : Boolean := True;
      --  An assignment can be invalid.

      Agents : Agent.Containers.Maps.Map;
   end record;

end Sancta.Assignment;
