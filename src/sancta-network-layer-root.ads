with Ada.Containers.Doubly_Linked_Lists,
     Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Streams,
     Agpl.Average_Queue,
     Agpl.Average_Queue.Timed,
     Agpl.Xml;
with Sancta.Network.Packets;

use Ada.Streams;

pragma Elaborate_All (Agpl.Average_Queue.Timed);

package Sancta.Network.Layer.Root is

   --  Common functionality that can be reused by many network layers
   --  This is implemented internally as a protected type, so
   --  derived types are thread safe if they do likewise.

   Log_Section : constant String := "sancta.network.layer.root";
   Bw_Section  : constant String := "sancta.network.layer.root.bandwidth";

   ----------------------------------------------
   --  OPTIONS TO CONFIGURE THE NETWORK LAYER  --
   ----------------------------------------------

   type Options (<>) is tagged private;

   type Options_Access is access constant Options;

   procedure Set_Asynchronous_Dispatch
     (This    : in out Options;
      Enabled :        Boolean := False);
   --  Asynchronous dispatch uses a thread for each call to a listener, which
   --  prevents one listener to blocking the rest, but is thread-heavy

   procedure Set_Count_Own_BW
     (This    : in out Options;
      Enabled :        Boolean := False);
   --  Include packets to self in BW computations

   --------------------------------------------
   --  THE NETWORK LAYER COMMON BASE OBJECT  --
   --------------------------------------------

   type Object is abstract limited new Layer.Object with private;

   type Object_Access is access all Object'Class;

   not overriding
   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node) is abstract;
   --  Generic initializer from XML data.

   function Config (This : Object'Class) return Options;
   --  This gets current config so you can update it; this is the only way
   --  to obtain a configuration object...

   procedure Set_Config (This : in out Object'Class; Config : Options);

   not overriding
   procedure Set_Id (This : in out Object;
                     Id   :        Node_Id);

   not overriding
   procedure Start_Listening (This : in out Object);
   --  Call this once you've set all up for Receive to work.
   --  This is non-blocking, since it just releases a thread.

   overriding
   function Id (This : Object) return Node_Id;

   overriding
   procedure Send (This : in out Object;
                   Dest : in     Address;
                   Data : in     Message'Class);
   --  Broadcast and Multicast iterate over receivers.
   --  If the network provides something better, override this one.
   --  Will dispatch to the one that takes a stream element array, which is the
   --    one that a layer implementor must override.

   not overriding
   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array) is abstract;
   --  Override this one for the final sending of bits.

   Interrupted_Reception : exception;
   Shutting_Down         : exception;

   not overriding
   function Receive
     (This : Object) return Stream_Element_Array is abstract;
   --  This blocking call must wait for the reception of a message.
   --  May raise Interrupted_Reception when no message can be returned.
   --  May raise Shutting_Down to signal that the network is going down.
   --  Override this to provide the specific network layer receive mechanism.

   not overriding
   procedure Clear_Nodes (This : in out Object);
   --  Clear address tables...

--     not overriding
--     procedure Add_Node
--       (This : in out Object;
--        Node :        Node_Id);
--  Mark that a node exists in order for broadcast to work.
--  Superseded by the one with Protocol_Specific_Address

   overriding
   procedure Add_Node
     (This : in out Object;
      Node :        Node_Id;
      Addr :        Protocol_Specific_Address);
   --  This is needed to be able to reverse-solve ids to addresses.
   --  If overriden, call this too!

   not overriding
   procedure Delete_Node (This : in out Object; Node : Node_Id);

   overriding
   function Get_Node_Id (This : Object;
                         Addr : Protocol_Specific_Address)
                         return Node_Id;

   overriding
   function Get_Address (This : Object;
                         Id   : Node_Id) return Protocol_Specific_Address;

   not overriding
   function Contains (This : Object; Id : Node_Id) return Boolean;
   --  Node exists and is routable

   overriding
   function Num_Nodes (This : Object) return Natural;
   --  Nodes that have been configured via Add_Node

   overriding
   function Node_Names (This : Object) return Node_Set;

   not overriding
   function Everybody (This : Object) return Group_Id;
   --  Get broadcast group

   not overriding
   procedure Add_Node_To_Group
     (This     : in out Object;
      Node     :        Node_Id;
      Group    :        Group_Name);
   --  Management of groups within the network layer, for simplicity.

   not overriding
   function Get_Group (This  : Object;
                       Group : Group_Name) return Group_Id;

   overriding
   procedure Subscribe
     (This     : in out Object;
      Listener :        Consumer.Object_Access;
      Chan     :        Channel);

   overriding
   procedure Shutdown (This : in out Object);
   --  Will remove subscriptors; it is a good idea to call this on
   --   child Shutdowns.

   function Avg_Bw_In (This : Object'Class) return Float;

   function Avg_Bw_Out (This : Object'Class) return Float;

private

   use Sancta.Network.Packets;

   use Ada.Containers;

   task type Receiver (Parent : not null access Object) is
      pragma Storage_Size (4 * Max_Message_Size);

      entry Start;
   end Receiver;

   package Consumer_Lists is new Doubly_Linked_Lists
     (Consumer.Object_Access,
      Consumer."=");

   package Consumer_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Channel,
      Consumer_Lists.List,
      "<", Consumer_Lists."=");
   --  We'll use a map of lists to keep track of consumers, indexed by tag.

   package Group_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Group_Name, Group_Id);

   subtype Bw_Unit is Float;

   package Avg_Static is new Agpl.Average_Queue (Bw_Unit);
   package Avg_Bw     is new Avg_Static.Timed;

   task type Bw_Informer (Parent : access Object);

   package Id_Addr_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Node_Id, Protocol_Specific_Address);

   package Addr_Id_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Protocol_Specific_Address, node_id);

   not overriding
   procedure Dispatch_To_Subscribers
     (This : Object;
      Cons : Consumer_Maps.Map;
      P    : Packet);
   --  Dispatch a received packet to subscribers

   type Options is tagged record
      Async_Dispatch : Boolean := False;
      Count_Own_BW   : Boolean := False; -- Include in BW usage packets to self
   end record;

   protected type Safe_Type (Parent : access Object) is

      function Get_Id return Node_Id;
      procedure Set_Id (Id : Node_Id);

      function Get_Everybody return Group_Id;
      function Get_Group (Group : Group_Name) return Group_Id;
      function Get_Consumers return Consumer_Maps.Map;
      procedure Clear_Consumers;

--        procedure Add_Node (Node : Node_Id);
      procedure Add_Node (Node : Node_Id; Addr : Protocol_Specific_Address);
      procedure Delete_Node (Node : Node_Id);
      function Contains (Node : Node_Id) return Boolean;
      procedure Clear_Nodes;

      function Get_Node_Id (Addr : Protocol_Specific_Address)
                            return Node_Id;
      function Get_Address (Id   : Node_Id) return Protocol_Specific_Address;

      procedure Add_Node_To_Group (Node : Node_Id; Group : Group_Name);
      procedure Delete_Node_From_Group (Node : Node_Id; Group : Group_Name);

      procedure Subscribe
        (Listener :        Consumer.Object_Access;
         Chan     :        Channel);

      function Num_Nodes return Natural;

      function Node_Names return Node_Set;

      function Get_Config return Options_Access;

      procedure Set_Config (Config : Options);

   private

      Id        : Node_Id := No_Node;       -- Self
      Everybody : Group_Id;

      Consumers : Consumer_Maps.Map;        -- Subscribed objects
      Groups    : Group_Maps.Map;

      Id_Addr   : Id_Addr_Maps.Map;
      Addr_Id   : Addr_Id_Maps.Map;

      Config    : aliased Options;

   end Safe_Type;

   type Object is abstract limited new Layer.Object with
      record
         Self      : access Object := Object'Unchecked_Access;

         Safe      : Safe_Type (Object'Access);
         Recv      : Receiver (Object'Access); -- Receiver

         Bw_In,
         Bw_Out    : Avg_Bw.Object (Slots => 5, Slot_Duration => 1000);
         Bw_Task   : Bw_Informer (Object'Access);

         The_End   : Boolean := False;
      end record;

end Sancta.Network.Layer.Root;
