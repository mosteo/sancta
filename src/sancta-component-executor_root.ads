with Ada.Finalization,
     Sancta.Agent,
     Sancta.Component.Root,
     Sancta.Containers,
     Sancta.Tasks;

package Sancta.Component.Executor_Root is

   --  A component to derive from, for task executors.
   --  Not to be instantiated directly.
   --  Will fetch the first task in an agent, and pass it along conveniently.
   --  Outputs a task whenever it's completed.

   Log_Section : constant String := "sancta.component.executor_root";

   Requires_Agent     : aliased constant Internal_Key := "agent";
   --  Descendents need not to parse this, it's done here.

   Provides_Task_Done : constant Internal_Key := "task";
   --  This is Outputted here, no need to do it in descendants unless
   --   some change in logic requires it.

   Provides_Task_List : constant Internal_Key := "task_list";
   --  Outputted everything a change in the list happens: either additions or
   --   removals, or the task under execution has changed.

   Option_Period  : constant Option_Attr  := "period";
   --  Period of calls, in seconds (Duration)
   --  Descendents need not to parse this, it's done here.

   type Object (Name   : access constant String;
                Config :                 Comp_Config)
   is abstract new Root.Object with private;
   --  After creation, Agent and Period will have been initialized.

   not overriding
   procedure Execute (This : in out Object;
                      Job  : in out Tasks.Object'Class;
                      Done :    out Boolean) is abstract;
   --  Done must be true when the task has been finished.

   not overriding
   procedure Idle (This : in out Object) is null;
   --  Called when the monitored agent has no tasks

   not overriding
   function Get_Agent (This : Object) return access Sancta.Agent.Object'Class;

private

   use Sancta.Containers;

   Name : aliased constant Component_Name := "executor_root";

   type Object_Preparer (Parent : access Object) is limited new
     Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (Thix : in out Object_Preparer);

   type Object (Name   : access constant String;
                Config :                 Comp_Config)
   is abstract new Root.Object (Name, Config) with record
      Bot    : access Sancta.Agent.Object'Class;
      Prev   :        Tc.Lists.List;
      Period :        Duration := 0.01;

      Prep   :        Object_Preparer (Object'Access);
   end record;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Executor_Root;
