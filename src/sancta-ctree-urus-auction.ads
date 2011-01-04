with Sancta.Auctions,
     Sancta.Plans.Portable;

use Sancta;

package Sancta.Ctree.Urus.Auction is

   --  pragma preelaborate;

   type Items is new Sancta.Auctions.Items with record
      Mission : Plans.Portable.Plan;
      Node    : Plans.Portable.Cursor;
   end record;
   --  We always send the full mission this auction belongs to, even if we
   --    are auctioning a partial plan.

   overriding
   function Id (This : Items) return Auctions.Auction_Id;

end Sancta.Ctree.Urus.Auction;
