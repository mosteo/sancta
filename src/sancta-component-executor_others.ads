with Sancta.Component.Executor_Root,
     Sancta.Tasks,
     Sancta.Types;

package Sancta.Component.Executor_Others is

   --  Execution of Explore_Directed_Segment

   Log_Section : constant String := "sancta.component.executor_others";

   Name : aliased constant Component_Name := "executor_others";

   Requires_Agent : Internal_Key renames Component.Executor_Root.Requires_Agent;
   Requires_Pose  : constant Internal_Key := "pose";

   Provides_Task_Done : Internal_Key renames
     Component.Executor_Root.Provides_Task_Done;

   Provides_Goal      : constant Internal_Key := "goal";

   Option_Distance  : constant Option_Attr := "distance"; -- for reaching goals
   Default_Distance : constant Sancta.Types.Real := 1.0;

   Option_Period    : Option_Attr renames Component.Executor_Root.Option_Period;

   procedure Register;

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config) is
     new Executor_Root.Object (Name, Config) with record
      Distance : Sancta.Types.Real := Default_Distance;
   end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Execute (This : in out Object;
                      Job  : in out Tasks.Object'Class;
                      Done :    out Boolean);

end Sancta.Component.Executor_Others;
