with Sancta.Component.Root;

--  This component will observe a variable and kill the process if said var isn't
--  updated in the timeout given.

package Sancta.Component.Watchdog is

   Name            : aliased constant String := "watchdog";

   Requires_Var    : constant Internal_Key := "var";

   Option_Timeout  : constant Option_Attr := "timeout";
   Default_Timeout : constant Duration := 5.0;

   procedure Register;

private

   type Object;
   type Object_Access is access all Object;

   task type Timer_Task (Parent : access Object) is
      entry Reset;
      entry Shutdown;
      entry Start;
   end Timer_Task;

   type Object is new Root.Object with record
      Timer   : Timer_Task (Object'Access);
      Timeout : Duration := Default_Timeout;
   end record;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   overriding
   procedure Stop (This : in out Object);

end Sancta.Component.Watchdog;
