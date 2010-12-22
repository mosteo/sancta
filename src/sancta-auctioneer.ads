--  An agent that just auctions tasks and always awards them

with Ada.Containers.Vectors;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Auctions;
with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Network.Layer;

with Ada.Calendar,
     Agpl.Containers.Indefinite_Multiordered_Maps;

use Ada.Calendar;

package Sancta.Auctioneer is

--   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.auctioneer";

   Default_Deadline     : constant Duration := 1.0;
   Default_Randomize    : constant Boolean  := False;

   type Object (Link : not null access Network.Layer.Object'Class)
     is new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   type Item_Info is record
      Id    : Auctions.Auction_Id;
      Image : Ustring;
   end record;

   package Item_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Item_Info);

   subtype Item_Info_Vector is Item_Info_Vectors.Vector;

   type Item_Info_Array is array (Positive range <>) of Item_Info;

   procedure Create (This         : out Object;
                     Chan         :     Network.Channel;
                     Deadline     :     Duration := Default_Deadline;
                     Random_Order :     Boolean  := Default_Randomize);

   procedure Add_Item
     (This    : in out Object;
      Item    : in     Sancta.Auctions.Items'Class;
      Bidders :        Positive := Positive'Last);
   --  A new item to auction.
   --  After timer expires, it is awarded if some bid was received,
   --  or added to the tail of pending auctions otherwise.
   --  If enough bidders answer, the auction ends immediately

   not overriding
   function Allow_Award
     (This  : access Object;
      Item  : access Auctions.Items'Class) return Boolean;
   --  By default, all won auctions are awarded.
   --  This can be overriden to prevent the final awarding of a task if for
   --  some reason the situation has changed since auction start

   type Actions_When_No_Winner is (Retry, Discard);

   not overriding
   function On_No_Winner
     (This  : access Object;
      Item  : access Auctions.Items'Class) return Actions_When_No_Winner;
   --  Defaults to Retry, override for specific behavior...

   procedure Run (This : in out Object);
   --  Must be called periodically (it's not blocking, nor launchs a task)

   function Pending_Items (This : Object) return Item_Info_Array;

   procedure Pending_Items (This  :        Object;
                            Items : in out Item_Info_Vector);

private

   type Auction_States is (Waiting, Ready, Running, Finished);
   --  Waiting: waiting to start
   --  Ready:  ready to be auctioned
   --  Running: waiting bids
   --  Finished: auction awarded, awaiting deletion

   type Auction is record
      Status   : Auction_States := Waiting;

      Item     : Auctions.Item_Handle.Object;
      Start    : Time := Clock;

      Best_Bid : Costs   := Infinite;
      Best_Id  : Node_Id := No_Node;

      Bid_Count        : Natural  := 0;
      Bid_Target_Count : Positive := Positive'Last;
   end record;

   type Key_Kinds is (By_Time, By_Id);
   type Key (Kind : Key_Kinds := By_Time) is record
      case Kind is
         when By_Time =>
            Arrival : Time := Clock;
         when By_Id =>
            Id      : Auctions.Auction_Id;
      end case;
   end record;

   function "<" (L, R : Key) return Boolean; pragma Inline ("<");

   package Auction_Maps is new Agpl.Containers.Indefinite_Multiordered_Maps
     (key, Key_Kinds, Auction);

--   package Auction_Lists is new Ada.Containers.Doubly_Linked_Lists (Auction);

   type Object (Link : not null access Network.Layer.Object'Class) is
   new Netlistener.Object (Link) with record
      Auctions  : Auction_Maps.Map;

      Backoff      : Time     := Clock;
      Deadline     : Duration := Default_Deadline;
      Randomize    : Boolean  := False;
      Chan         : Network.Channel;
   end record;

   procedure Delete_Marked (This : in out Object);

   procedure Enable_Random_Auction (This : in out Object);
   --  Check that no auction is running, and select a random paused one to
   --  be moved to ready state.

   overriding
   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

end Sancta.Auctioneer;
