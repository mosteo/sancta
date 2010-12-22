--  An agent that just auctions tasks and always awards them

package body Sancta.Auction_Messages is

   -------------------------
   -- Create_Offer_Packet --
   -------------------------

   function Create_Offer_Packet (Job     : in Sancta.Tasks.Object'Class;
                                 Dur     : in Duration;
                                 Max_Bid : in Credits := Types.Infinite;
                                 Must    : in Boolean  := False)
                                 return Offer_Message
   is
   begin
      return (Network.Message with
              Dur       => Dur,
              Max_Bid   => Max_Bid,
              Job       => Sancta.Tasks.Handle.Set (Job),
              Must_Bid  => Must);
   end Create_Offer_Packet;

   -----------------------
   -- Create_Bid_Packet --
   -----------------------

   function Create_Bid_Packet (Id   : in Sancta.Tasks.Task_Id;
                               Bid  : in Credits)
                               return Bid_Message
   is
   begin
      return (Network.Message with
              Id        => Id,
              Bid       => Bid);
   end Create_Bid_Packet;

   -------------------------
   -- Create_Award_Packet --
   -------------------------

   function Create_Award_Packet (Job : in Sancta.Tasks.Object'Class)
                                 return   Award_Message
   is
   begin
      return (Network.Message with
              Job => Sancta.Tasks.Handle.Set (Job));
   end Create_Award_Packet;

end Sancta.Auction_Messages;
