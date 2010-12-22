with Ada.Calendar,
     Sancta.Auctions,
     Sancta.Bidder;

use Sancta;

package T004_Auctions_Types is

   type Item is new Auctions.Items with record
      Now : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   overriding function Id (This : Item) return Auctions.Auction_Id;
   overriding function Image (This : Item) return String;

   type Bidder is new Sancta.Bidder.Object with null record;

   overriding procedure Compute_Bid (This : in out Bidder;
                                     Item :        Auctions.Items'Class;
                                     Bid  :    out Costs;
                                     Ok   :    out Boolean);

   overriding procedure Win (This : in out Bidder;
                             Item :        Auctions.Items'Class) is null;

end T004_Auctions_Types;
