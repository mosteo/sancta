

--  Just implements listening for network messages.
--  Extend it for other purposes.

with Sancta.Network;
with Sancta.Network.Inbox;
with Sancta.Network.Layer;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization; use Ada.Finalization;
with Ada.Tags;

--  This objects allows two ways of handling received packets, both of them
--  out of the receiving loop (so the network link won't hold while you use
--  them).

--  THE POINT OF THIS CLASS IS TO DECOUPLE THE RECEIVING OF PACKETS FROM THE
--  NETWORK LAYER, SO IT DON'T WAIT WHILE WE PROCESS A PACKET!

--  One is to override the Process_Incoming_Packet
--  The other is to register a callback procedure.

package Sancta.Netlistener is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.netlistener";
   Detail_Section : constant String := "sancta.netlistener.detail";
   --  To selectively enable debug messages...

   type Object
     (Link  : not null access Network.Layer.Object'Class) is
   abstract tagged limited private;

   type Object_Access is access all Object'Class;

   type Callback is access procedure (This : in out Object'Class;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

   procedure Null_Callback (This : in out Object'Class;
                            M    : in     Network.Message'Class;
                            Meta : in     Network.Message_Metadata) is null;
   --  Usable to de-register some callback...

   procedure Init (This : in out Object) is null;
      --  You can override this with subscription to required messages.
      --  This is called during object instantiation!

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is null;
   --  Do something with a received packet.
   --  Override for the packets of your interest.
   --  All packets to channels being listened are passed here.

   procedure Run (This : in out Object);
   --  Run a control step. Should be called periodically.
   --  It causes calls to callbacks or Process_Incoming_Packet.
   --  If you extend it, you should call the parent Run to keep
   --  current functionality.

   --  Subscriptions cause the messages to be routed to the node.
   --  Registrations set handlers for these messages.
   --  So you need to both subscribe to some channels
   --  and register callbacks (or override Process_Incoming_Packet)

   procedure Register (This : in out Object;
                       Kind : in     Ada.Tags.Tag;
                       Cb   : in     Callback);
   --  Register a callback to be called upon some message reception.
   --  Only the last registered handler for a Tag will be called!!
   --  This allows message dispatching by tag within a channel.

   procedure Subscribe (This  : in out Object;
                        Chan  : in     Network.Channel);

private

   package Callback_Maps is new Ada.Containers.Indefinite_Ordered_Maps
        (String,
         Callback);

   type Object
     (Link  : not null access Network.Layer.Object'Class) is
   abstract new Limited_Controlled with record
      Mailbox   : aliased Network.Inbox.Object;
      Callbacks :         Callback_Maps.Map;

      Subscribed : Boolean := False;
   end record;

   procedure Initialize (This : in out Object);

end Sancta.Netlistener;
