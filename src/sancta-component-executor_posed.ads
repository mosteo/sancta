with Sancta.Agent,
     Sancta.Component.Environment,
     Sancta.Component.Executor_Root,
     Sancta.Tasks,
     Sancta.Tasks.Handle,
     Sancta.Types;

package Sancta.Component.Executor_Posed is

   --  Takes an agent, and if the first task is positioned, goes to it.
   --  Outputs an idle agent, which will have just one task once the posed
   --  goal has been reached.
   --  Hence, other executors can daisy-chain on this agent to provide the
   --  actual execution at the pose.

   --  As a bonus, for Goto_Pose tasks, no daisy-chaining is needed since this
   --  one will consider it done once the pose is reached.

   Log_Section : constant String := "sancta.component.executor_posed";

   Name : aliased constant Component_Name := "executor_posed";

   Requires_Agent : Internal_Key renames Component.Executor_Root.Requires_Agent;
   Requires_Pose  : constant Internal_Key := "pose";
   --  Bear in mind that the goals generated are *IN THE SAME REF. FRAME* than
   --   this pose is. So local/global converters may be needed at some point.

   Provides_Task_Done : Internal_Key renames
     Component.Executor_Root.Provides_Task_Done;
   Provides_Goal      : constant Internal_Key := "goal";
   Provides_Velo      : constant Internal_Key := "velo";
   Provides_Agent     : constant Internal_Key := "out_agent";
   --  See above about this.

   Option_Period   : Option_Attr renames Component.Executor_Root.Option_Period;

   Option_Distance  : constant Option_Attr := "distance";
   Default_Distance : constant Types.Real := 1.0;

   procedure Register;
   --  This plugin is created on demand

private

   type States is (Going_To_Pose, Waiting_Task_Completion, Idle);

   type Object (Name   : access constant String;
                Config :                 Comp_Config) is
     new Executor_Root.Object (Name, Config) with record
      Out_Agent : Sancta.Agent.Object_Access;

      Status    :        States     := Idle;
      Dist      :        Types.Real := Default_Distance;
      Job       :        Tasks.Handle.Object;

      Stopped   :        Boolean := False;
   end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    :    Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Execute (This : in out Object;
                      Job  : in out Tasks.Object'Class;
                      Done :    out Boolean);

   overriding
   procedure Idle (This : in out Object);

end Sancta.Component.Executor_Posed;
