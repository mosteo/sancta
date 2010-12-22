with Sancta.Component.Root;

private with Agpl.Chronos;

package Sancta.Component.Task_Completion_Checker is

   Log_Section : constant String := "sancta.component.task_completion_checker";

   Name : aliased constant Component_Name := "task_completion_checker";

   Requires_In_Tasks  : constant Internal_Key := "in_tasks";
   Requires_Agent     : constant Internal_Key := "agent";
   --  Optional; its first task will be checked.
   Requires_Team      : constant Internal_Key := "team";
   --  Assigment; optional; each first task will be checked.
   Provides_Out_Tasks : constant Internal_Key := "out_tasks";
   --  In_Tasks minus completed ones

   Option_Shutdown  : constant Option_Attr := "shutdown";
   --  If given and true, will invoke Sancta.Starter.Shutdown on empty tasks.
   Option_Shutdown_Delay : constant Option_Attr := "shutdown_delay";
   Default_Shutdown_Delay : constant Duration := 1.0;

   procedure Register;

private

   use Agpl;

   type Object is new Root.Object with record
      Timer_Shutdown : Chronos.Object;
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Task_Completion_Checker;
