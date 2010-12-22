--  Sample empty component to save-as when creating new ones.

with Agpl.Tasking.Period,
     Sancta.Component.Root;

with Player.Client,
     Player.Interfaces;

package Sancta.Component.Player_Client is

   --  Connect to a player device.
   --  Uses poll mode with replacement rule.
   --  Other needs, code it yourself!

   Name : aliased constant Component_Name := "player_client";

   Log_Section : constant String := "sancta.component.player_client";

   Option_Host : constant Option_Attr := "host";
   --  Host to connect to; defaults to localhost
   Option_Port : constant Option_Attr := "port";
   --  Port, defaults to 6665

   Option_Period  : constant Option_Attr := "period";
   Default_Period : constant Duration    := 0.1;

   Provides_Connection   : constant Internal_Key := "connection";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;


   type Iface_Access is access all Player.Interfaces.Object'Class;

   --  Auxiliary type for safe player code execution.
   --  This is needed because we want all player code run in mutual exclusion
   type Player_Code is abstract tagged null record;
   procedure Run (This : in out Player_Code;
                  Conn : in out Player.Client.Object) is abstract;

   protected type Safe_Object (This : access Object'Class) is
      procedure Sync;
      --  Request latest data from server

      procedure Execute (Code : in out Player_Code'Class);
      procedure Execute (Code : access procedure);
      procedure Execute (Code  : access procedure (Iface : Iface_Access);
                         Iface : Iface_Access);
      --  Synchronous execution of code
   end Safe_Object;
   type Safe_Access is access all Safe_Object;

   type Connection (Ref : Safe_Access) is new Data with null record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;
   --  The connection is available after this call, so interface components
   --  can retrieve it on creation for subscription or whatever

   procedure Register;

private

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);
   --  This will run a periodic data pull.
   --  Clients needing finer resolution should manually call Sync.

   overriding
   procedure Stop (This : in out Object);

   overriding
   function Requires_Mutex (This : Object) return Boolean;

   use Agpl;

   type Object is new Root.Object with record
      Safe : aliased Safe_Object (Object'Access);
      Conn : aliased Player.Client.Object;

      Period : Tasking.Period.Object := Tasking.Period.Create (Default_Period);
   end record;

end Sancta.Component.Player_Client;
