with Agpl.Trace,
     Sancta.Insertion;

use Agpl.Trace;

package body Sancta.Bidder.Flat is

   ------------
   -- Create --
   ------------

   procedure Create
     (This           : in out Object;
      Chan           :        Network.Channel;
      Award_Deadline :        Duration := Default_Award_Deadline;
      Crit           :        Criteria.Assignment_Criteria := Default_Criterion)
   is
   begin
      Bidder.Object (This).Create (Chan, Award_Deadline);
      This.Criterion := Crit;
   end Create;

   -----------------
   -- Compute_Bid --
   -----------------

   procedure Compute_Bid
     (This : in out Object;
      Item :        Auctions.Items'Class;
      Bid  :    out Costs;
      Ok   :    out Boolean)
   is
   begin
--        if not This.Cost.Contains (This.Bot.Get_Name,
--                                   Sancta.Tasks.No_Task,
--                                   Flat.Item (Item).Job.Ref.Get_Id)
--        then
         This.Cost.Add_Costs (This.Bot.all,
                              Flat.Item (Item).Job.Ref.all);
--        end if;

      declare
         Insert : constant Insertion.Bids :=
                    Insertion.Auction (This.Bot.all,
                                       Flat.Item (Item).Job.Ref.all,
                                       This.Criterion,
                                       This.Cost.all);
      begin
         Ok := Insert.Can_Win;

         if Ok then
            Bid := Insert.Get_Cost;
         end if;
      end;
   end Compute_Bid;

   ---------
   -- Win --
   ---------

   procedure Win
     (This : in out Object;
      Item :        Auctions.Items'Class)
   is
      Insert : constant Insertion.Bids :=
                 Insertion.Auction (This.Bot.all,
                                    Flat.Item (Item).Job.Ref.all,
                                    This.Criterion,
                                    This.Cost.all);
   begin
      if not Insert.Can_Win then
         Log ("Won task can't be inserted", Error, Log_Section);
         raise Constraint_Error with "Won task can't be inserted";
      else
         Log ("Won: " & Flat.Item (Item).Job.Ref.Image,
              Informative, Log_Section);
         This.Bot.Set_Tasks (Insertion.Apply (This.Bot.all, Insert).Get_Tasks);
      end if;
   end Win;

   --------
   -- Id --
   --------

   function Id (This : Item) return Auctions.Auction_Id is
   begin
      return Auctions.Id (This.Image);
   end Id;

   -----------
   -- Image --
   -----------

   function Image (This : Item) return String is
   begin
      return This.Job.Ref.Image;
   end Image;

end Sancta.Bidder.Flat;
