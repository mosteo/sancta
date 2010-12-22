with Agpl.Chronos,
     Sancta.Agent,
     Sancta.Component.Root,
     Sancta.Containers;

package Sancta.Component.Agent_To_Tasks is

   --  Extract the task list in an agent an publish changes
   --  It doesn't publish changes in individual tasks (only compares by ID)
   --  Obsoleted by Executor_Root, which publishes the task list on both changes

   Log_Section : constant String := "sancta.component.agent_to_tasks";

   Name : aliased constant Component_Name := "agent_to_tasks";

   Requires_Agent : constant Internal_Key := "agent";
   Provides_Tasks : constant Internal_Key := "tasks";

   Option_Period  : constant Option_Attr  := "period";
   Default_Period : constant Duration     := 0.01;

   type Object (<>) is new Root.Object with private;

   procedure Register;

private

   use Sancta.Containers;

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Agent  : access Sancta.Agent.Object'Class)
     is new Root.Object (Name, Config) with
      record
         Timer  : Agpl.Chronos.Object;
         Period : Duration := Default_Period;
         Prev   : Tc.Lists.List;
      end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Agent_To_Tasks;
