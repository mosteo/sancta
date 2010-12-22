with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Agpl.Chronos;
with Agpl.Tasking.Period;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Component.Root;
with Sancta.Network.Consumer;
with Sancta.Network.Layer.Root;

package Sancta.Component.Dns is

   --  Auto-updates nodes in a Sancta network with node ids
   --  Either by periodic broadcast or on demand (not yet implemented)

   Log_Section : constant String := "sancta.component.dns";
   Det_Section : constant String := "sancta.component.dns.detail";

   Name : aliased constant Component_Name := "dns";

   Requires_Link : constant Internal_Key := "link";

   Requires_Address : constant Internal_Key := "address";
   --  Not mandatory. If provided, the reported address will be this instead
   --    of the attribute below "Self"

   --  NOTE: the master node has to have a static address, that must be provided
   --    as both the self and master addresses

   Opt_Master_Address : constant Option_Attr := "master_address";
   --  This is a Network_Specific_Address, not a Node_Id!
   --  This way, not even the master node is needed at the Network XML definition

   Opt_Self_Address : constant Option_Attr := "self_address";
   --  Our Network_Specific_Address

   Opt_Self_Groups  : constant Option_Attr := "self_groups";
   Def_Self_Groups  : constant String := "";
   --  Space separated list of group names this node belongs to

   Opt_Channel : constant Option_Attr := "channel";
   --  Where to listen for messages

   Opt_Purge_Period : constant Option_Attr := "purge_period";
   Def_Purge_Period : constant Duration    := 10.0;
   --  After this time, a node not heard from is marked as dead

   Opt_Announce_Period : constant Option_Attr := "announce_period";
   Def_Announce_Period : constant Duration    := 5.0;
   --  Period for nodes to announce themselves and master to publish full network.

   Opt_Broadcast : constant Option_Attr := "broadcast";
   Def_Broadcast : constant Boolean     := True;

   Dns_Master_Node : constant Node_Id := Value ("dns_master_node");
   --  This is internally used, no node should have this id... or bad things
   --   will happen

   type Object (<>) is new Root.Object and Sancta.Network.Consumer.Object
   with private;

   procedure Register;

private

   package Node_Ages is new
     Ada.Containers.Ordered_Maps (Node_Id, Agpl.Chronos.Object,
                                  "<", Agpl.Chronos."=");

   type Node_Info is record
      Id     : Node_Id;
      Addr   : Ustring;
      Groups : Ustring;
   end record;

   function "<" (L, R : Node_Info) return Boolean;

   package Node_Sets is new
     Ada.Containers.Ordered_Sets (Node_Info);

   type Message_Kind is (Node_Update,
                         Master_Partial_Update,
                         Master_Full_Update);
   --  Node_Update: a node sends its address and groups
   --  Master_Partial_Update: the master sends a new 1st time heard node data
   --  Master_Full_Update: the master sends the whole routing table

   type Message (Kind : Message_Kind) is new Network.Message with record
      case Kind is
         when Node_Update =>
            Client : Node_Info;
         when Master_Partial_Update =>
            New_Client : Node_Info;
         when Master_Full_Update =>
            Nodes : Node_Sets.Set;
      end case;
   end record;

   protected type Safe_Type (Parent : access Object) is

      procedure Init;

      procedure Process (Msg : Message; Link : Network.Layer.Root.Object_Access);

      procedure Run (Next : out Ada.Calendar.Time);

      procedure On_Reception
        (M    : in     Sancta.Network.Message'Class;
         Meta : in     Sancta.Network.Message_Metadata);

      function Is_Master return Boolean;

      procedure Add_Node (Node : Node_Info);

      function Self_Address return String;

      function Has_Self_Address return Boolean;

   private

      Self_Added : Boolean := False;

      Nodes   : Node_Sets.Set;
      Ages    : Node_Ages.Map;
      Timer   : Agpl.Tasking.Period.Object;
      Channel : Sancta.Network.Channel;

   end Safe_Type;

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : Sancta.Network.Layer.Root.Object_Access) is
     new Root.Object (Name => Name, Config => Config)
     and Sancta.Network.Consumer.Object
   with record
      Safe : Safe_Type (Object'Access);
   end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure On_Reception (This : in out Object;
                           M    : in     Sancta.Network.Message'Class;
                           Meta : in     Sancta.Network.Message_Metadata);

   not overriding
   function Do_Broadcast (This : Object) return Boolean;

   not overriding
   function Purge_Period (This : Object) return Duration;

end Sancta.Component.Dns;
