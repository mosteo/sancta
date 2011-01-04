--  A simple bidder that uses a flat task list. I.E. it expects all tasks to
--  be primitive.

--  It takes care of adding missing costs in the cache, asking the agent

--  For explore_directed_segment, it considers flipping the task

with Sancta.Agent,
     Sancta.Auctions,
     Sancta.Bidder.Flat,
     Sancta.Cost_Cache,
     Sancta.Network.Layer;

use Sancta;

package Sancta.Ctree.Ad_Hoc_Bidder is

   Log_Section : constant String := "nerus.ad_hoc_bidder";

   type Object (Link : not null access Network.Layer.Object'Class;
                Cost : not null access Cost_Cache.Object'Class;
                Bot  : not null access Agent.Object'Class)
     is new Sancta.Bidder.Flat.Object with private;

private

   type Object (Link : not null access Network.Layer.Object'Class;
                Cost : not null access Cost_Cache.Object'Class;
                Bot  : not null access Agent.Object'Class)
     is new Sancta.Bidder.Flat.Object (Link, Cost, Bot) with record
      Cached_Item : Auctions.Item_Handle.Object;
   end record;

   overriding
   procedure Compute_Bid (This : in out Object;
                          Item :        Auctions.Items'Class;
                          Bid  :    out Costs;
                          Ok   :    out Boolean);
   --  Override this to provide the actual bidding.
   --  If not OK, no answer will be sent and Bid is discarded.

   overriding
   procedure Win (This : in out Object;
                  Item :        Auctions.Items'Class);

end Sancta.Ctree.Ad_Hoc_Bidder;
