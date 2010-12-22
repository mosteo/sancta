with Sancta.Component.Environment;
with Sancta.Component.Executor_Root,
     Sancta.Tasks;

package Sancta.Component.Executor_Manual_Driving is

   --  Execution of speed driving, wait for order tasks

   Log_Section : constant String := "sancta.component.executor_manual_driving";

   Name : aliased constant Component_Name := "executor_manual_driving";

   Requires_Agent : Internal_Key renames Component.Executor_Root.Requires_Agent;
   Requires_Pose : constant Internal_Key := "pose";

   Provides_Task_Done : Internal_Key renames
     Component.Executor_Root.Provides_Task_Done;

   Provides_Goal      : constant Internal_Key := "goal";
   Provides_Velo      : constant Internal_Key := "velo";

   Option_Period   : Option_Attr renames Component.Executor_Root.Option_Period;

   procedure Register;
   --  This component is created on demand

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config) is
     new Executor_Root.Object (Name, Config) with record
      Stopped : Boolean := False;
   end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    : Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Execute (This : in out Object;
                      Job  : in out Tasks.Object'Class;
                      Done :    out Boolean);

end Sancta.Component.Executor_Manual_Driving;
