--  Implementation using regular UDP.

with Agpl.Xml;
with Gnat.Sockets;
with Sancta.Network.Layer.Root;

package Sancta.Network.Layer.Udp is

   --  UDP specific address format for nodes is
   --  x.x.x.x:p

   --  So, several nodes can coexist in one machine

   Log_Section : constant String := "sancta.network.layer.udp";

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

   not overriding
   procedure Initialize (This : in out Object;
                         Port :        Positive);

   use Ada.Streams,
       Gnat.Sockets;

   overriding
   function Receive (This : Object) return Stream_Element_Array;

   overriding
   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array);

   type Object is limited new Root.Object with record
      Sock  : Socket_Type;       -- Our socket
      Port  : Port_Type;
   end record;

end Sancta.Network.Layer.Udp;
