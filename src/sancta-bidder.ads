--  An object that answers to bid messages.
--  For generic items; the actual negotiation must be carried in desdendants.
--  For single auctions.

with Ada.Calendar;
with Sancta.Auctions;
with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Network.Layer;

package Sancta.Bidder is

   Default_Award_Deadline : constant Duration := 1.5;
   Default_Can_Win_Own    : constant Boolean  := False;
   Default_Bid_On_Cant_Win: constant Boolean  := True;

   Log_Section : constant String := "sancta.bidder";

   type Object (Link : not null access Network.Layer.Object'Class)
   is abstract new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   not overriding
   procedure Create
     (This            : in out Object;
      Chan            :        Network.Channel;
      Award_Deadline  :        Duration := Default_Award_Deadline;
      Can_Win_Own     :        Boolean  := Default_Can_Win_Own;
      Bid_On_Cant_Win :        Boolean  := Default_Bid_On_Cant_Win);
   --  Can win own says if we can win something we're auctioning

   not overriding
   procedure Compute_Bid (This : in out Object;
                          Item :        Auctions.Items'Class;
                          Bid  :    out Costs;
                          Ok   :    out Boolean) is abstract;
   --  Override this to provide the actual bidding.
   --  If not OK, an infinite bid will be sent

   not overriding
   procedure Win (This : in out Object;
                  Item :        Auctions.Items'Class) is abstract;

   overriding
   procedure Run (This : in out Object);
   --  Must be called periodically (it's not blocking, nor launchs a task)

private

   use Ada.Calendar;

   type Object (Link : not null access Network.Layer.Object'Class) is
   abstract new Netlistener.Object (Link) with record
      Chan           : Network.Channel;
      Item           : Auctions.Item_Handle.Object;
      Received       : Time     := Clock;
      Award_Deadline : Duration := Default_Award_Deadline;
      Can_Win_Own    : Boolean  := Default_Can_Win_Own;
      Bid_On_Cant_Win: Boolean  := Default_Bid_On_Cant_Win;
   end record;

   overriding
   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

end Sancta.Bidder;
