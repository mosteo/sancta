

package body Sancta.Bidder is

   ------------
   -- Create --
   ------------

   procedure Create
     (This           : in out Object;
      Chan           :     Network.Channel;
      Award_Deadline :     Duration := Default_Award_Deadline;
      Can_Win_Own    :     Boolean  := Default_Can_Win_Own;
      Bid_On_Cant_Win :     Boolean  := Default_Bid_On_Cant_Win)
   is
   begin
      This.Subscribe (Chan);
      This.Chan           := Chan;
      This.Award_Deadline := Award_Deadline;
      This.Can_Win_Own    := Can_Win_Own;
      This.Bid_On_Cant_Win := Bid_On_Cant_Win;
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object) is
   begin
      --  Netlistener doings
      Netlistener.Run (Netlistener.Object (This));

      --  Check for oblivioned auction
      if This.Item.Is_Valid and then
         Clock - This.Received > This.Award_Deadline
      then
         Log (Image (This.Link.Id) &
              ": Cleaning expired item", Debug, Log_Section);
         This.Item.Clear;
      end if;
   end Run;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
   begin
      if M in Auctions.Message'Class then
         declare
            use Auctions;
            Msg : Auctions.Message renames Auctions.Message (M);
         begin
            if Meta.Receiver.Intended_For (This.Link.Id) then
               case Msg.Kind is
               when Auctions.Offer =>
                  -- OFFER --
                  if not This.Can_Win_Own and then Meta.Sender = This.Link.Id then
                     Log (Image (This.Link.Id) & ": Discarded OFFER [self] from " &
                          Image (Meta.Sender), Debug, Log_Section);
                  elsif This.Item.Is_Valid then
                     Log (Image (This.Link.Id) & ": Discarded OFFER [busy] from " &
                          Image (Meta.Sender), Debug, Log_Section);
                  else
                     declare
                        Bid : Costs;
                        Ok  : Boolean := False;
                     begin
                        Log (Image (This.Link.Id) & ": Received OFFER [idle] from "
                             & Image (Meta.Sender), Debug, Log_Section);
                        Object'Class (This).Compute_Bid
                          (Msg.Item.Ref.all, Bid, Ok);

                        if Ok then
                           This.Item     := Msg.Item;
                           This.Received := Clock;

                           Log (Image (This.Link.Id) & ": Sending BID: " &
                                Bid'Img, Debug, Log_Section);
                           This.Link.Send_Async
                             (Network.New_Address (Meta.Sender, This.Chan),
                              Auctions.Message'(Kind => Auctions.Bid,
                                                Id   => Msg.Id,
                                                Bid  => Bid));
                        elsif This.Bid_On_Cant_Win then
                           This.Item     := Msg.Item;
                           This.Received := Clock;

                           Log (Image (This.Link.Id) & ": Sending BID: " &
                                "INF", Debug, Log_Section);
                           This.Link.Send_Async
                             (Network.New_Address (Meta.Sender, This.Chan),
                              Auctions.Message'(Kind => Auctions.Bid,
                                                Id   => Msg.Id,
                                                Bid  => Sancta.Infinite));
                        else
                           Log (Image (This.Link.Id) &
                                ": Couldn't get bid, passing.",
                                Debug, Log_Section);
                           --  Couldn't do it:
                           This.Item.Clear;
                        end if;
                     end;
                  end if;
               when Auctions.Award =>
                  -- AWARD --
                  if Msg.Winner = This.Link.Id then
                     if This.Item.Is_Valid and then This.Item.Ref.Id = Msg.Id then
                        Log (Image (This.Link.Id) &
                             ": Received AWARD [win]", Debug, Log_Section);
                        Object'Class (This).Win (This.Item.Ref.all);
                        This.Item.Clear;
                     else
                        Log (Image (This.Link.Id) &
                             ":  AWARD [unexpected, no auction running]",
                             Debug, Log_Section);
                     end if;
                  elsif This.Item.Is_Valid and then Msg.Id = This.Item.Ref.Id then
                     Log (Image (This.Link.Id) &
                          ": Received AWARD [loss]", Debug, Log_Section);
                     This.Item.Clear;
                  end if;
               when Auctions.Bid =>
                  null; -- not of our interest.
               end case;
            end if;
         end;
      end if;
   end Process_Incoming_Packet;

end Sancta.Bidder;
