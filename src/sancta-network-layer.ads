--  The purpose of having an abstract network layer is to have a pluggable way
--  of using either TCP, UDP or an ad hoc transport layer.

with Sancta.Network.Consumer;

package Sancta.Network.Layer is

--     pragma Preelaborate;

   type Object is limited interface;
   --   type Object (Id : access Node_Id) is synchronized interface;
   --   A bug in gpl'06 prevents the usage of this feature.
   --  Subprograms of this kind of object should be thread-safe.
   --  Id says who's the one associated with this relay.
   --  As a stopgag measure, Layer.Root is a protected type to derive from.
   --  Note that anyway we can't inherit from protecteds, so perhaps it's
   --  better this way...

   type Object_Access is access all Object'Class;

   function Id (This : Object) return Node_Id is abstract;

   procedure Send (This : in out Object;
                   Addr :        Address;
                   Data :        Message'Class) is abstract;

   subtype Milliseconds is Natural;

   procedure Send_Async (This : in out Object'Class;
                         Dest : in     Address;
                         Data : in     Message'Class;
                         Wait : in     Milliseconds := 0;
                         Stack: in     Natural      := 2 * Max_Message_Size);
   --  Use this to send asynchronously.
   --  Note that this uses a temporary worker thread.

   procedure Send (This : in out Object'Class;
                   Chan :        Channel;
                   Data :        Message'Class);
   --  Simple wrapper for broadcast to a channel

   procedure Shutdown (This : in out Object) is abstract;
   --  Clean shutdown of the network layer.
   --  Close sockets, notify leaving, whatever.

   procedure Add_Node
     (This : in out Object;
      Node :        Node_Id;
      Addr :        Protocol_Specific_Address) is abstract;
   --  A protocol-specific address is passed with its equivalence as Node_Id.
   --  This will be used during network initialization.

   function Get_Node_Id (This : Object;
                         Addr : Protocol_Specific_Address)
                         return Node_Id is abstract;

   function Get_Address (This : Object;
                         Id   : Node_Id)
                         return Protocol_Specific_Address is abstract;

   function Num_Nodes (This : Object) return Natural is abstract;

   function Node_Names (This : Object) return Node_Set is abstract;
   --  Return all known node names

   procedure Subscribe (This     : in out Object;
                        Listener :        Consumer.Object_Access;
                        Chan     :        Channel) is abstract;
   --  Several consumers should be possible.

end Sancta.Network.Layer;
