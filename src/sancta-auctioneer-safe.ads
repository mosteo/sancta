package Sancta.Auctioneer.Safe is

   protected type Object is

      procedure Create (Unsafe       : Object_Access;
                        Chan         : Network.Channel;
                        Deadline     : Duration := Default_Deadline;
                        Random_Order : Boolean  := Default_Randomize);
      --  The unsafe object should not be used anywhere outside here!

      procedure Add_Item (Item    : Sancta.Auctions.Items'Class;
                          Bidders : Positive := Positive'Last);
      --  If enough bidders answer, the auction ends immediately

      procedure Run;

      function Pending_Items return Item_Info_Array;

      procedure Pending_Items (Items : in out Item_Info_Vector);

   private

      Auc : Object_Access;

   end Object;

end Sancta.Auctioneer.Safe;
