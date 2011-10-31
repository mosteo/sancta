with Sancta.Agent,
     Sancta.Component.Environment,
     Sancta.Component.Root,
     Sancta.Netlistener,
     Sancta.Network,
     Sancta.Network.Layer;

package Sancta.Component.Task_Listener is

   Log_Section : constant String := "sancta.component.task_listener";

   --  Listens for network messages related to task management

   Name : aliased constant Component_Name := "task_listener";

   Requires_Agent : aliased constant Internal_Key := "agent";
   --  The agent whose task list we are going to alter

   Requires_Link  : aliased constant Internal_Key := "link";

   Option_Channel : constant Option_Attr  := "channel";
   --  Channel in which to listen

   procedure Register;

private

   type Object;

   type Listener_Type (Link   : not null access Network.Layer.Object'Class;
                       Parent : access Object) is
     new Netlistener.Object (Link) with null record;

   overriding
   procedure Process_Incoming_Packet (This : in out Listener_Type;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class;
                Agent  : access Sancta.Agent.Object'Class)
     is new Root.Object (Name, Config) with
      record
         Listener : Listener_Type (Link, Object'Access);
      end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    : Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Task_Listener;
