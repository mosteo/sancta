

--  A basic interactive agent.

with Sancta.Plan;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Primitive;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

--  with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Tags;

package Sancta.Agent is

   pragma Preelaborate;

   package Tc renames Sancta.Tasks.Containers;

   type Object is abstract tagged private;

   type Object_Access is access all Object'Class;

   function "=" (L, R : Object) return Boolean;
   --  Equality is determined by agent name by default

   procedure Add_Task
     (This : in out Object; The_Task : in Sancta.Tasks.Object'Class);
   --  Assign this task to the end of this agent TO DO list.

   procedure Add_Tasks (This : in out Object; T : Tc.Lists.List);
   --  Calls to Add_Task, no need to override.

   procedure Clear_Tasks (This : in out Object);
   --  Remove all assigned tasks.

   function Finished (This : Object;
                      Job  : Tasks.Object'Class) return Boolean;
   --  Say if a task has been finished
   --  Default raises program error

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean) is abstract;
   --  Called periodically to allow the agent to perform the required
   --  control actions related with a primitive task.
   --  Changing the plan can cause replanning.
   --  Done must be set to true when the task is finished.

   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Sancta.Plan.Object);
   --  Will be called when no tasks pending.
   --  Default does nothing.

   function Get_Cached_Cost (This : in Object) return Costs;
   --  Returns the cost that was estimated when starting execution.

--     function Get_Elapsed (This : in Object) return Duration;
--     --  Returns the elapsed time since starting execution (Mark_Start).

   function Get_Cost (This : in Object; The_Task : in Sancta.Tasks.Object'Class)
                      return Costs;
   --  Must say how takes for the agent to do The_Task from its current pose.
   --  This default will call Get_Cost (From, To) with From being instance of
   --  Sancta.Tasks.Starting_Pose for the agent.
   --  Unless The_Task is already of Starting_Pose'Class, when this returns 0.0.
   --  In practice, this means you have to implement Get_Cost (From, To) taking
   --  into account that From can be a Starting_Pose task.

   function Get_Cost (This : in Object; From, To : in Sancta.Tasks.Object'Class)
                      return Costs;
   --  Must say how takes for the robot to do To assuming his last assignment
   --  was From.
   --  This default return Costs'Last.
   --  Should return Costs'Last for undoable tasks, never raise Constraint_Error.

   function Get_Plan_Cost (This : in Object) return Costs;
   --  Says the cost of doing everything in the TO DO list.
   --  Should return Costs'Last for undoable plans, never raise Constraint_Error.

   function Get_Plan_Cost (This  : in Object;
                           Tasks : in Sancta.Tasks.Containers.Lists.List) return Costs;
   --  Says the cost of this plan, not the one in the robot.
   --  Should return Costs'Last for undoable plans, never raise Constraint_Error.

   function Get_Name (This : in Object) return String;
   function Get_Name2 (This : in Object'Class) return String;

   function Get_Without_Tasks (This : in Object) return Object'Class;
   --  A copy of himself but without tasks

   function Get_First_Task (This : in Object) return Sancta.Tasks.Object'Class;

   function Get_Last_Task  (This : in Object) return Sancta.Tasks.Object'Class;

   function Get_Task_Count (This : in Object) return Natural;

   function Get_Tasks (This : in Object)
                       return Sancta.Tasks.Containers.Lists.List;

   procedure Check_Task_List (This     : Object;
                              Checker  : access procedure
                                (Tasks : Sancta.Tasks.Containers.Lists.List));
   --  Will call Checker with the in-place task list, to avoid
   --  redundant copying.

   procedure Modify_Task_List (This     : in out Object;
                               Modifier : access procedure
                                 (Tasks : in out Sancta.Tasks.Containers.Lists.List));
   --  Will call Modifier with a in-place modifiable task list, to avoid
   --  redundant copying.

   procedure Set_Task (This : in out Object;
                       T    :        Sancta.Tasks.Object'Class);
   --  Removes any other task currently assigned.

   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Sancta.Tasks.Containers.Lists.List);
   --  Set all the ordered tasks that must conform the TO DO list;

   function Has_Tasks (This : in Object) return Boolean;
   --  Says if the TO DO list is not empty.

--     procedure Mark_Start (This : in out Object);
   --  Marks the execution is starting now, and caches the estimated cost.

   procedure Remove_First_Task (This : in out Object);

   procedure Remove_Task (This : in out Object; Id : in Sancta.Tasks.Task_Id);
   --  Remove this task from this agent TO DO list.

   procedure Set_Name (This : in out Object; Name : in String);

   procedure Print_Plan_Cost (This : in Object);
   --  For debugging.

   --------------------
   --  TASK HANDLERS --
   --------------------
   type Task_Executer is access procedure (This : in out Object'Class;
                                           Job  : in out Sancta.Tasks.Object'Class;
                                           Done : in out Boolean);

   procedure Add_Task_Executer (This : in out Object;
                                Job  : in     Ada.Tags.Tag;
                                Exe  : in     Task_Executer);
   --  Add a new executer for a task kind.

   procedure Call_Executer (This : in out Object;
                            Job  : in out Sancta.Tasks.Object'Class;
                            Done : in out Boolean);
   --  No need to override this. It simply dispatchs to a registered executer.

   function Has_Executer (This : in Object;
                          Job  : in Sancta.Tasks.Object'Class) return Boolean;
   --  Says if there's a registered executer for this task.

   procedure Set_Not_Before (This : in out Object;
                             Pos  :        Natural);

   function Get_Not_Before (This : Object) return Natural;

private

   package Executer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Task_Executer, Ada.Strings.Hash, "=", "=");

   type Object is abstract tagged record
      Name  : Ustring;
      --  Mnemonic for the agent.

      Cost  : Costs;
--      Start : Ada.Calendar.Time;

      Tasks : Sancta.Tasks.Containers.Lists.List;
      --  The TO DO list for this agent.

      Execs : Executer_Maps.Map;
      --  Registered task executers for this agent.

      Not_Before : Natural := 0;
      --  Worst hack *EVER* for preventing task insertion in auctions
      --  before a certain list position
      pragma Shameful("Remove this hack my god");
   end record;

end Sancta.Agent;
