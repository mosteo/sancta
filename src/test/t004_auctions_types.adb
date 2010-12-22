with Agpl.Calendar.Format,
     Agpl.Random;

use Agpl;

package body T004_Auctions_Types is

   --------
   -- Id --
   --------

   function Id (This : Item) return Auctions.Auction_Id is
   begin
      return Auctions.Id (Agpl.Calendar.Format.Timestamp (This.Now));
   end Id;

   -----------
   -- Image --
   -----------

   function Image (This : Item) return String is
   begin
      return Agpl.Calendar.Format.Timestamp (This.Now);
   end Image;

   -----------------
   -- Compute_Bid --
   -----------------

   procedure Compute_Bid (This : in out Bidder;
                          Item :        Auctions.Items'Class;
                          Bid  :    out Costs;
                          Ok   :    out Boolean)
   is
      pragma Unreferenced (This, Item);
   begin
      Ok  := True;
      Bid := Costs (Random.Uniform);
   end Compute_Bid;

end T004_Auctions_Types;
