with Sancta.Component.Root;

package Sancta.Component.Task_Watchdog is

   --  Monitors a task list outputting its emptyness

   Name : aliased constant Component_Name := "task_watchdog";

   Requires_Tasks : constant Internal_Key := "tasks";

   Provides_Flag  : constant Internal_Key := "flag";
   --  Flag boolean = Tasks.Is_Empty

   type Object is new Root.Object with null record;

   type Object_Access is access all Object'Class;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   procedure Register;

private

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Task_Watchdog;
