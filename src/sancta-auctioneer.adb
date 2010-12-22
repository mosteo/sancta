--  An agent that just auctions tasks and always awards them

with Agpl.Conversions;
with Agpl.Random;
with Agpl.Trace;

use Agpl;

package body Sancta.Auctioneer is

   use Network;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Key) return Boolean is
      use Auctions;
   begin
      case L.Kind is
         when By_Id   => return L.Id      < R.Id;
         when By_Time => return L.Arrival < R.Arrival;
      end case;
   end "<";

   ------------
   -- Create --
   ------------

   procedure Create (This         : out Object;
                     Chan         :     Network.Channel;
                     Deadline     :     Duration := Default_Deadline;
                     Random_Order :     Boolean  := Default_Randomize)
   is
   begin
      This.Chan         := Chan;
      This.Subscribe (Chan);
      This.Deadline     := Deadline;
      This.Randomize    := Random_Order;
   end Create;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (This    : in out Object;
      Item    : in     Sancta.Auctions.Items'Class;
      Bidders :        Positive := Positive'Last)
   is
      Now : constant Time := Clock;
   begin
      This.Auctions.Include
        (Key     => (By_Time => (By_Time, Now),
                     By_Id   => (By_Id, Item.Id)),
         Element => (Item   => Auctions.Item_Handle.Set (Item),
                     Status => Waiting,
                     Bid_Count => 0,
                     Bid_Target_Count => Bidders,
                     others => <>));
      Log (Image (This.Link.Id) & ": Added item for auction [" &
           Item.Readable_Id & "]", Trace.Debug, Log_Section);
   end Add_Item;

   -------------------
   -- Delete_Marked --
   -------------------

   procedure Delete_Marked (This : in out Object) is
      use Auction_Maps;
      I : Cursor'Class := This.Auctions.Last (By_Time);
      X : Cursor'Class := I;
   begin
      while Has_Element (I) loop
         X := Previous (I);
         if Element (I).Status = Finished then
            This.Auctions.Delete (I);
         end if;
         I := X;
      end loop;
   end Delete_Marked;

   ---------------------------
   -- Enable_Random_Auction --
   ---------------------------

   procedure Enable_Random_Auction (This : in out Object) is
      package Auction_Vectors is
        new Ada.Containers.Vectors (Positive,
                                    Auctions.Auction_Id,
                                    Auctions."=");

      Available : Auction_Vectors.Vector;

      use Auction_Maps;
      I : Cursor'Class := This.Auctions.First (By_Id);

      use Auction_Vectors;
   begin
      --  Do not start auction if we are still backing off from
      --   hearing another one.
      if This.Backoff > Clock then
         return;
      end if;

      while Has_Element (I) loop
         case Element (I).Status is
            when Ready =>
               raise Constraint_Error; -- Should never be in this state
            when Running =>
               return; -- EXITING, IF AUCTION RUNNING WE CAN'T START ANOTHER ONE
            when Finished =>
               null;
            when Waiting =>
               Available.Append (Auction_Maps.Key (I).Id);
         end case;
         Next (I);
      end loop;

      if Available.Is_Empty then
         return;
      end if;

      declare
         Target : Auction renames
           This.Auctions.Element
             (By_Id,
              (Kind => By_Id,
               Id   => Available.Element
                 (Random.Get_Integer
                    (First_Index (Available),
                     Last_Index  (Available))))).all;
      begin
         pragma Assert (Target.Status = Waiting);
         Target.Status := Ready;
      end;
   end Enable_Random_Auction;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
      use Auctions;
      Msg : Auctions.Message renames Auctions.Message (M);
      function Image (C : Costs) return String is
      begin
         if C = Infinite then
            return "INF";
         else
            return To_String (C);
         end if;
      end Image;
   begin
      case Msg.Kind is
         when Bid =>
            -- BID --
            if Meta.Receiver.Intended_For (This.Link.Id) then
               Log (Image (This.Link.Id) &
                    ": Received bid from " & Image (Meta.Sender) &
                    " with cost " & Image (Msg.Bid),
                    Trace.Debug, Section => Log_Section);
               declare
                  use Auction_Maps;
                  X : constant Cursor :=
                        Cursor (This.Auctions.Find (By_Id, (By_Id, Msg.Id)));
               begin
                  if Has_Element (X) then
                     X.Element.all.Bid_Count := X.Element.all.Bid_Count + 1;
                     if Msg.Bid < X.Element.Best_Bid then
                        X.Element.all.Best_Bid := Msg.Bid;
                        X.Element.all.Best_Id  := Meta.Sender;
                     end if;
                  else
                     Log (Image (This.Link.Id) & ": Received bid from " &
                          Image (Meta.Sender) &
                          " for unknown auction : " & Auctions.Image (Msg.Id),
                          Trace.Debug, Section => Log_Section);
                  end if;
               end;
            else
               Log (Image (This.Link.Id) & ": Received unintended bid for " &
                    Network.Image (Meta.Receiver), Warning, Log_Section);
            end if;
         when Offer =>
            if Meta.Receiver.Intended_For (This.Link.Id) and then
               Meta.Sender /= This.Link.Id
            then
               Log (Image (This.Link.Id) &
                    ": Backing off, other auction heard from " &
                    Image (Meta.Sender), Debug, Log_Section);
               This.Backoff :=
                 Clock + This.Deadline * Duration (1.0 + Random.Uniform);
            end if;
         when Award =>
            null; -- These are only relevant to bidders.
      end case;
   end Process_Incoming_Packet;

   -------------------
   -- Pending_Items --
   -------------------

   function Pending_Items (This : Object) return Item_Info_Array is
      Aucs : Item_Info_Array (1 .. Natural (This.Auctions.Length));
      I    : Positive := Aucs'First;

      procedure Check (K : Key; Auc : Auction) is
         pragma Unreferenced (K);
      begin
         if Auc.Status /= Finished then
            Aucs (I) := (Id    => Auc.Item.Ref.Id,
                         Image => +Auc.Item.Ref.Image);
            I := I + 1;
         end if;
      end Check;

   begin
      This.Auctions.Iterate_Query (Check'Access);
      return Aucs (Aucs'First .. I - 1);
   end Pending_Items;

   -------------------
   -- Pending_Items --
   -------------------

   procedure Pending_Items (This  :        Object;
                            Items : in out Item_Info_Vector)
   is
      procedure Check (K : Key; Auc : Auction) is
         pragma Unreferenced (K);
      begin
         if Auc.Status /= Finished then
            Items.Append ((Id    => Auc.Item.Ref.Id,
                           Image => +Auc.Item.Ref.Image));
         end if;
      end Check;

   begin
      Items.Clear;
      This.Auctions.Iterate_Query (Check'Access);
   end Pending_Items;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object) is
      use Auction_Maps;

      One_Running : Boolean := False;

      -------------------
      -- Check_Auction --
      -------------------

      procedure Check_Auction (K : Key; X : in out Auction) is
         pragma Unreferenced (K);
      begin
         if One_Running and then X.Status = Ready then
            return;
         end if;

         case X.Status is
            when Waiting   =>
               null;
            when Finished =>
               null;
            when Ready =>
               if not One_Running then
                  One_Running := True;
                  X.Status := Running;
                  This.Link.Send_Async
                    (Network.New_Address (This.Chan),
                     Auctions.Message'
                       (Kind    => Auctions.Offer,
                        Id      => X.Item.Get.Id,
                        Item    => X.Item,
                        Max_Bid => X.Best_Bid));
                  X.Start := Clock;
                  Log (Image (This.Link.Id) & ": Started auction for " &
                       X.Item.Get.Readable_Id,
                       Informative, Section => Log_Section);
               else
                  raise Program_Error with "Two ready/running auctions?";
               end if;
            when Running =>
               One_Running := True;

               if X.Status /= finished and then
                 (Clock - X.Start > This.Deadline or else
                    X.Bid_Count >= X.Bid_Target_Count)
               then
                  if X.Bid_Count >= X.Bid_Target_Count then
                     Log ("Premature auction end by target bidders reached",
                          Debug, Log_Section);
                  else
                     Log ("Auction ended by timeout",
                          Debug, Log_Section);
                  end if;

                  --  Auction ended:
                  X.Status := Finished;
                  One_Running := False;

                  if X.Best_Id /= No_Node and then
                    Object'Class (This).Allow_Award (X.Item.Ref)
                  then
                     --  Award the auction
                     Log (Image (This.Link.Id) & ": Awarding item " &
                          X.Item.Get.Readable_Id & " to " &
                          Image (X.Best_Id) & " with cost " &
                          To_String (X.Best_Bid),
                          Informative, Section => Log_Section);
                     --  Broadcast award so others can know the auction ended
                     This.Link.Send_Async
                       (Network.New_Address (This.Chan),
                        Auctions.Message'(Kind   => Auctions.Award,
                                          Winner => X.Best_Id,
                                          Id     => X.Item.Get.Id));
                     X.Status := Finished;
                  else
                     case Object'Class (This).On_No_Winner (X.Item.Ref) is
                        when Retry =>
                           --  requeue it after sending a fake award message
                           Log (Image (This.Link.Id) & ": Requeuing item " &
                                X.Item.Get.Readable_Id, Informative, Log_Section);
                           X.Status := Waiting;

                        when Discard =>
                           --  requeue it after sending a fake award message
                           Log (Image (This.Link.Id) & ": Discarding item " &
                                X.Item.Get.Readable_Id, Informative, Log_Section);
                           X.Status := Finished;

                     end case;

                     This.Backoff :=
                       Clock + This.Deadline * Duration (0.0 + Random.Uniform);
                     --  To be in equal ground with other Auctioneers.
                     This.Link.Send_Async
                       (Network.New_Address (This.Chan),
                        Auctions.Message'(Kind   => Auctions.Award,
                                          Winner => No_Node,
                                          Id     => X.Item.Get.Id));
                  end if;
               end if;
         end case;
      end Check_Auction;

   begin
      --  Netlistener doings
      Netlistener.Run (Netlistener.Object (This));

      --  Select a random auction to be started
      This.Enable_Random_Auction;

      --  Check auctions
      This.Auctions.Iterate_Update (Check_Auction'Access, By_Time);

      --  Remove deleted
      This.Delete_Marked;
   end Run;

   -----------------
   -- Allow_Award --
   -----------------

   function Allow_Award
     (This  : access Object;
      Item  : access Auctions.Items'Class) return Boolean
   is
      pragma Unreferenced (This, Item);
   begin
      return True;
   end Allow_Award;

   ------------------
   -- On_No_Winner --
   ------------------

   function On_No_Winner
     (This : access Object;
      Item : access Auctions.Items'Class) return Actions_When_No_Winner is
      pragma Unreferenced (This, Item);
   begin
      return Retry;
   end On_No_Winner;

end Sancta.Auctioneer;
