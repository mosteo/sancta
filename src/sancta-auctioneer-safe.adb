package body Sancta.Auctioneer.Safe is

   ------------
   -- Object --
   ------------

   protected body Object is

      ------------
      -- Create --
      ------------

      procedure Create
        (Unsafe       : Object_Access;
         Chan         : Network.Channel;
         Deadline     : Duration := Default_Deadline;
         Random_Order : Boolean  := Default_Randomize)
      is
      begin
         Auc := Unsafe;
         Auc.Create (Chan, Deadline, Random_Order);
      end Create;

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Item    : Sancta.Auctions.Items'Class;
                          Bidders : Positive := Positive'Last) is
      begin
         Auc.Add_Item (Item, Bidders);
      end Add_Item;

      ---------
      -- Run --
      ---------

      procedure Run is
      begin
         Auc.Run;
      end Run;

      -------------------
      -- Pending_Items --
      -------------------

      function Pending_Items return Item_Info_Array is
      begin
         return Auc.Pending_Items;
      end Pending_Items;

      -------------------
      -- Pending_Items --
      -------------------

      procedure Pending_Items (Items : in out Item_Info_Vector) is
      begin
         Auc.Pending_Items (Items);
      end Pending_Items;

   end Object;

end Sancta.Auctioneer.Safe;
