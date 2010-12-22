--  A simple bidder that uses a flat task list. I.E. it expects all tasks to
--  be primitive.

--  It takes care of adding missing costs in the cache, asking the agent

with Sancta.Agent,
     Sancta.Auctions,
     Sancta.Cost_Cache,
     Sancta.Criteria,
     Sancta.Tasks.Handle;

package Sancta.Bidder.Flat is

   Log_Section : constant String := "sancta.bidder.flat";

   Default_Criterion : constant Criteria.Assignment_Criteria :=
                         Criteria.Criterion_Minmax;

   type Object (Link : not null access Network.Layer.Object'Class;
                Cost : not null access Cost_Cache.Object'Class;
                Bot  : not null access Agent.Object'Class)
     is new Bidder.Object with private;

   not overriding
   procedure Create
     (This           : in out Object;
      Chan           :        Network.Channel;
      Award_Deadline :        Duration := Default_Award_Deadline;
      Crit           :        Criteria.Assignment_Criteria := Default_Criterion);

   overriding
   procedure Compute_Bid (This : in out Object;
                          Item :        Auctions.Items'Class;
                          Bid  :    out Costs;
                          Ok   :    out Boolean);

   overriding
   procedure Win (This : in out Object;
                  Item :        Auctions.Items'Class);

   type Item is new Auctions.Items with record
      Job : Tasks.Handle.Object;
   end record;

   overriding
   function Id (This : Item) return Auctions.Auction_Id;
   pragma Inline (Id);

   overriding
   function Image (This : Item) return String;
   pragma Inline (Image);

private

   type Object (Link : not null access Network.Layer.Object'Class;
                Cost : not null access Cost_Cache.Object'Class;
                Bot  : not null access Agent.Object'Class)
     is new Bidder.Object (Link => Link) with
      record
         Criterion : Criteria.Assignment_Criteria := Criteria.Criterion_Minmax;
      end record;

end Sancta.Bidder.Flat;
