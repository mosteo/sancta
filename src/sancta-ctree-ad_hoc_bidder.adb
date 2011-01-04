with Sancta.Tasks.Explore_Directed_Segment;

package body Sancta.Ctree.Ad_Hoc_Bidder is

   package Explore    renames Sancta.Tasks.Explore_Directed_Segment;
   package Parent_Pkg renames Sancta.Bidder.Flat;

   -----------------
   -- Compute_Bid --
   -----------------

   procedure Compute_Bid
     (This : in out Object;
      Item :        Auctions.Items'Class;
      Bid  :    out Costs;
      Ok   :    out Boolean)
   is
      Parent : Parent_Pkg.Object renames Parent_Pkg.Object (This);
      Job : Tasks.Object'Class renames
        Parent_Pkg.Item (Item).Job.Ref.all;
   begin
      if Job in Explore.Object then
         declare
            Bid1, Bid2 : Costs;
            Ok1, Ok2   : Boolean := False;
            Item2      : Parent_Pkg.Item := Parent_Pkg.Item (Item);
         begin
            Parent.Compute_Bid (Item, Bid1, Ok1);
            Item2.Job.Set (Explore.Object (Job).Flip);
            Parent.Compute_Bid (Item2, Bid2, Ok2);
            Ok := Ok1 or Ok2;
--            Log ("Bid/ok:" & Bid1'Img & Ok1'Img & Bid2'Img & Ok2'Img, Always);
            if Ok then
               Bid := Costs'Last;
               if Ok1 then
                  This.Cached_Item.Set (Item);
                  Bid := Bid1;
               end if;
               if Ok2 and then Bid2 < Bid then
                  This.Cached_Item.Set (Item2);
                  Bid := Bid2;
               end if;
            end if;
         end;
      else
         This.Cached_Item.Set (Item);
         Parent.Compute_Bid (Item, Bid, Ok);
      end if;
   end Compute_Bid;

   ---------
   -- Win --
   ---------

   procedure Win
     (This : in out Object;
      Item :        Auctions.Items'Class)
   is
      Parent : Parent_Pkg.Object renames Parent_Pkg.Object (This);
      Job : Tasks.Object'Class renames
        Parent_Pkg.Item (Item).Job.Ref.all;
   begin
      if Job in Explore.Object then
         declare
            Fake_Bid : Costs;
            Fake_Ok  : Boolean;
         begin
            This.Compute_Bid (Item, Fake_Bid, Fake_Ok);
            if Fake_Ok then
--                 Log ("Winning: " & Parent_Pkg.Item
--                      (This.Cached_Item.Ref.all).Job.Ref.Image, Always);
               Parent.Win (This.Cached_Item.Ref.all);
            else
               Log ("Cannot allocate won auction!", Warning, Log_Section);
            end if;
         end;
      else
         Parent.Win (Item);
      end if;
   end Win;

end Sancta.Ctree.Ad_Hoc_Bidder;
