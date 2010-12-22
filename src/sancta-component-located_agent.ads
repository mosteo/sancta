with Sancta.Agent_Proxy,
     Sancta.Component.Root;

package Sancta.Component.Located_Agent is

   --  Takes a pose and provides an agent.

   Log_Section : constant String := "sancta.component.located_agent";

   Name : aliased constant Component_Name := "located_agent";

   Option_Name    : constant Option_Attr  := "agent_name";

   Requires_Pose  : constant Internal_Key := "pose";
   Requires_Velo  : constant Internal_Key := "velo";

   Provides_Agent : constant Internal_Key := "agent";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   procedure Register;

private

   type Object is new Root.Object with record
      Agent : Agent_Proxy.Object_Access;
      Given : Boolean := False;
   end record;

   function Create (Config : Agpl.Xml.Node)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Located_Agent;
