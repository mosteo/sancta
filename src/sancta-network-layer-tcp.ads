--  Implementation using regular TCP.

with Agpl.Xml;
with Gnat.Sockets;
with Sancta.Network.Layer.Root;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;

package Sancta.Network.Layer.Tcp is

   --  TCP specific address format for nodes is
   --  x.x.x.x:p

   --  So, several nodes can coexist in one machine

   Log_Section : constant String := "sancta.network.layer.tcp";

   type Object is limited new Root.Object with private;
   type Object_Access is access all Object'Class;

   overriding
   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node);
   pragma Precondition (This.Id /= No_Node);
   --  Remember to call Set_Id previously.

   overriding
   procedure Shutdown (This : in out Object);

private

   use Ada.Streams,
       Gnat.Sockets;

   overriding
   function Receive (This : Object) return Stream_Element_Array;

   overriding
   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array);

   package Packet_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Stream_Element_Array);

   protected type Packet_Queue is
      procedure Put (Packet : Stream_Element_Array);
      entry Get_Size (Size : out Stream_Element_Count);
      entry Get      (Packet : out Stream_Element_Array);
      --  Should match the last Get_Size length
   private
      Packets : Packet_Lists.List;
   end Packet_Queue;

   package Id_Socket_Maps is new Ada.Containers.Ordered_Maps
     (Node_Id, Socket_Type);

   type Object is limited new Root.Object with record
      Self  : Object_Access := Object'Unchecked_Access;
      Dest  : Id_Socket_Maps.Map;
      Sock  : Socket_Type;       -- Our listening socket
      Port  : Port_Type := 0;
      Queue : Packet_Queue;
   end record;

end Sancta.Network.Layer.Tcp;
