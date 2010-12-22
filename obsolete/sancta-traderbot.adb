 

--  Implementation of traderbots for EXPRES.

pragma Warnings (Off);
with Sancta.Tasks.Used; -- To force inclusion of tasks code.
pragma Warnings (On);

with Sancta.Anneal;
with Sancta.Auction_Messages;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Tasks;
--  with Sancta.Types;
--  with Sancta.Types.Operations;

with Sancta.Agent;
with Sancta.Agent.Containers;
with Sancta.Assigner.Mtsp_Concorde;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Sancta.Tasks.Handle;
--  with Sancta.Tasks.Still;
with Agpl.Strings; use Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Traderbot is

   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;
   use type Auctioner.Cost_Policies;
   use type Network.Node_Id;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (This : in out Object;
                       That : in     Sancta.Tasks.Object'Class;
                       Keep : in     Boolean := False)
   is
      use Sancta.Tasks.Containers;
      use type Sancta.Tasks.Task_Id;

      Tasks  : constant Lists.List := This.Bot.Get_Tasks;
   begin
      Log ("Adding task " & That.To_String, Trace.Debug, Section => Log_Section);

      declare
         New_Tasks : constant Sancta.Tasks.Containers.Lists.List :=
                        Evaluate_Best_Task_List (This, Tasks, That);
      begin
         This.Bot.Set_Tasks (New_Tasks);

         --  Start auctions if necessary.
         if not Keep then
            if This.Auction_Task_Policy.Added then
               raise Program_Error;
               pragma Unimplemented;
               --  This.Do_Auction (AuctId); -- This will override any running auction!!!!
               --  If a full tasks auction is running,
               --  this will mess only the current one
               --  being offered.
            elsif This.Auction_Time_Policy.Auction_On_Adition then
               This.Do_Auction;
            end if;
         end if;
      end;
   end Add_Task;

   -------------------
   -- Award_Auction --
   -------------------

   procedure Award_Auction (This : in out Object) is
      use Sancta.Tasks.Containers;
      Ack : Boolean;
   begin
      --  Prevent awarding if forced auctions are being run from somewhere else.
      if This.Dont_Start_Auction_Timer.Elapsed <
        Auction_Messages.Dont_Start_Auction_Period
      then
         return;
      end if;

      --  Locate auctioned task
      declare
         use Lists;
         Tasks : Lists.List   := This.Bot.Get_Tasks;
         I     : Lists.Cursor := First (Tasks);
      begin
         while Has_Element (I) and then This.Task_To_Auction /= Element (I).Get_Id loop
            Next (I);
         end loop;

         if Has_Element (I) and then Element (I).Get_Id = This.Task_To_Auction then

            Log ("Awarding task " & Element (I).To_String & " to " &
                 Network.Image (This.Best_Bidder),
                 Trace.Debug, Section => Log_Section);

            This.Link.Send (Network.Value (Network.Image (This.Best_Bidder)),
                            Auction_Messages.Create_Award_Packet (Element (I)),
                            Tout => 1.0, Ack => Ack);

            if Ack then
               --  Remove the task from us:
               This.Cancel_Task (This.Task_To_Auction);
               Print (This.Bot.Get_Tasks);
            end if;

         else
            Log ("Auctioned task is missing", Warning, Section => Log_Section);
         end if;

      end;
   end Award_Auction;

   -----------------
   -- Cancel_Task --
   -----------------

   procedure Cancel_Task (This : in out Object;
                          Id   : in     Sancta.Tasks.Task_Id)
   is
   begin
      This.Bot.Remove_Task (Id);
   end Cancel_Task;

   ----------------------
   -- Continue_Auction --
   ----------------------

   procedure Continue_Auction (This : in out Object) is
      Auction_Done : Boolean := False;
   begin
      if not This.Auctioning then
         return;
      end if;

      if This.Bids_Received >= This.Max_Bidders then
         pragma Assert (This.Bids_Received = This.Max_Bidders);
         Auction_Done := True;
      elsif Clock > This.End_Of_Auction then
         Auction_Done := True;
      end if;

      if Auction_Done then
         This.Auctioning := False;
         This.Start_Auction_Timer.Reset;

         --  UGLY TRICK TO AVOID WRTITINDAGNSD SOME IFSS
         if This.Cost_Policy = Auctioner.Full_Cost then
            This.Best_Bidder := This.Full_Best_Bidder;
         end if;

         if This.Best_Bidder /= Network.No_Node then
            --  Send the AWARD message
            This.Award_Auction;
            --  If doing a full round...
            if This.Auctioning_All then
               This.Do_Auction;
               --  Since we have removed the pointed task, no need to advance pointer
            end if;
         else
            Log ("Auction ended without bidder doing better",
                 Trace.Debug, Section => Log_Section);
            --  If doing a full round...
            if This.Auctioning_All then
               This.Auctioning_Pos := This.Auctioning_Pos + 1;
               --  Because we have kept the task that was being auctioned.
               This.Do_Auction;
            end if;
         end if;
      end if;
   end Continue_Auction;

   ----------------
   -- Do_Auction --
   ----------------

   procedure Do_Auction (This : in out Object) is
      use Sancta.Tasks.Containers.Lists;
      Tasks : constant List := This.Bot.Get_Tasks;
      I     : Cursor  := Tasks.First;
      Pos   : Natural := 1;
   begin
      if not This.Auctioning_All then
         if Natural (This.Bot.Get_Tasks.Length) > 0 then
            This.Auctioning_All := True;
            This.Auctioning_Pos := 1;
            Log ("Starting full auction...", Trace.Debug, Section => Log_Section);
         end if;
      end if;

      if This.Auctioning or else
        This.Dont_Start_Auction_Timer.Elapsed <
          Auction_Messages.Dont_Start_Auction_Period
      then
         return; -- Wait for current auction to end
      end if;

      while Has_Element (I) loop

         --  Locate next to auction.
         while Pos < This.Auctioning_Pos and then Has_Element (I) loop
            Pos := Pos + 1;
            Next (I);
         end loop;

         --  Check if we are at a right position
         if Has_Element (I) and then -- So Pos = Auctioning_Pos
           Boolean'Value
             (Element (I).Get_Property
              (Sancta.Tasks.Property_Auctionable, "false"))
           and then
             ((This.Auctioning_Pos = 1 and then
                This.Auction_Task_Policy.First)
              or else (This.Auctioning_Pos = 2 and then
                  This.Auction_Task_Policy.Second)
              or else (This.Auctioning_Pos > 2 and then
                  Pos = This.Auctioning_Pos and then
                    This.Auction_Task_Policy.All_But_First_And_Second))
         then
            Log ("Located task for auction:" & Pos'Img,
                 Trace.Debug, Section => Log_Section);
            This.Do_Auction (Element (I).Get_Id);
            return;
         else
            --  Failure to start another auction. It may be because we're skipping
            --  the first, second or later tasks.
            --  So let's try again at next position:
            This.Auctioning_Pos := This.Auctioning_Pos + 1;

         end if;

      end loop;

      This.Auctioning_All := False; -- End of round
      if Natural (This.Bot.Get_Tasks.Length) > 0 then
         Log ("Full auction done.", Trace.Debug, Section => Log_Section);
      end if;
   end Do_Auction;

   ----------------
   -- Do_Auction --
   ----------------

   procedure Do_Auction (This : in out Object;
                         Id   : in     Sancta.Tasks.Task_Id)
   is
      use Sancta.Tasks.Containers.Lists;
      Tasks : List   := This.Bot.Get_Tasks;
      I     : Cursor := First (Tasks);
      Job   : Sancta.Tasks.Handle.Object; -- Will contain a copy of the task for sale.
      use Sancta.Tasks.Handle;
   begin
      This.Auctioning      := True;
      This.Best_Bid        := This.Bot.Get_Plan_Cost - This.Evaluate_Removing (Id);
      This.Best_Bidder     := Network.No_Node;
      This.Full_Best_Bid   := This.Bot.Get_Plan_Cost; -- Value to improve: current full plan.
      This.Full_Best_Bidder:= Network.No_Node;
      This.Bids_Received   := 0;
      This.End_Of_Auction  := Clock + This.Auction_Duration;
      This.Task_To_Auction  := Id;

      This.Task_To_Auction := Id;
      --  Locate the offered task:
      while Element (I).Get_Id /= Id loop
         Next (I);
      end loop;
      Set (Job, Element (I));

      Log ("Starting auction for task #" &
           To_String (Natural (Job.Get.Get_Id)) & " " & Job.Get.To_String,
           Trace.Debug, Section => Log_Section);

      --  Create and send the offering message:
      This.Link.Multicast
        (Network.Groups.Traderbot_Channel,
         Auction_Messages.Create_Offer_Packet (Job.Get,
                                        This.Auction_Duration,
                                        This.Best_Bid,
                                               This.Full_Best_Bid));
   exception
      when Cant_Integrate_Task =>
         Log ("Traderbot: Removing a task for auction generates invalid plan",
              Warning);
   end Do_Auction;

   ------------------------
   -- Evaluate_Best_Plan --
   ------------------------

   function Evaluate_Best_Plan (This : in Object;
                                Jobs : in Sancta.Tasks.Containers.Lists.List)
                                return    Sancta.Tasks.Containers.Lists.List
   is
      use Sancta.Tasks.Containers.Lists;
      Result  : List;
      Pending : List := Jobs;

   begin
      while not Is_Empty (Pending) loop
         declare
            Best_Cost : Sancta.Costs := Sancta.Costs'Last;
            Best_Pos  : Cursor   := No_Element;
            Best_Task : Sancta.Tasks.Handle.Object;

            procedure Select_Best is
               I         : Cursor            := Pending.First;
               Orig_Cost : constant Sancta.Costs :=
                             This.Bot.Get_Plan_Cost (Result);
            begin
               while Has_Element (I) loop
                  declare
                     Children : constant Sancta.Tasks.Containers.Vectors.Vector :=
                                  Anneal.Get_All_Children
                                    (Anneal.Parent
                                       (Sancta.Tasks.Primitive.Object'Class
                                          (Element (I))));
                  begin
                     for C in Children.First_Index .. Children.Last_Index loop
                        declare
                           Eval_Plan : List := Result;
                           Eval_Cost : Sancta.Costs;
                        begin
                           Eval_Plan.Append (Children.Element (C));

                           Eval_Cost := This.Bot.Get_Plan_Cost (Eval_Plan);

                           case This.Cost_Policy is
                           when Auctioner.Full_Cost =>
                              if Eval_Cost < Best_Cost then
                                 Best_Cost := Eval_Cost;
                                 Best_Pos  := I;
                                 Best_Task.Set (Children.Element (C));
                              end if;
                           when Auctioner.Local_Cost =>
                              if Eval_Cost - Orig_Cost < Best_Cost then
                                 Best_Cost := Eval_Cost - Orig_Cost;
                                 Best_Pos  := I;
                                 Best_Task.Set (Children.Element (C));
                              end if;
                           end case;
                        end;
                     end loop;
                  end;
                  Next (I);
               end loop;
            end Select_Best;
         begin
            Select_Best;
            if not Best_Task.Is_Valid then
               raise Cant_Integrate_Task;
            else
               Result.Append (Best_Task.Get);
               Pending.Delete (Best_Pos);
            end if;
         end;
      end loop;

      return Result;
   end Evaluate_Best_Plan;
   --  Create greedily a best plan considering task flips
   --  This is an ugly hack, will only flip once a task because we known the
   --  tasks under consideration are alternates.

   -----------------------
   -- Evaluate_Concorde --
   -----------------------

   function Evaluate_Concorde (This : in Object;
                               Jobs : in Sancta.Tasks.Containers.Lists.List)
                               return    Sancta.Tasks.Containers.Lists.List
   is
      package Tsp renames Sancta.Assigner.Mtsp_Concorde;
      Ag  : Sancta.Agent.Containers.Lists.List;
      Aux : Tsp.Object;
   begin

      if Natural (Jobs.Length) < 3 or else Natural (Jobs.Length) > 12 then
         return Jobs; -- Trivial solution.
      end if;

      Ag.Append (This.Bot.all);

      declare
         Ass : constant Sancta.Assignment.Object :=
                 Tsp.Assign (Aux,
                             Ag,
                             Jobs,
                             Sancta.Cost_Matrix.Create_With_Start (Ag, Jobs));
      begin
         return Ass.Get_Tasks (This.Bot.all);
      end;
   exception
      when E : others =>
         Log ("Traderbot.Evaluate_Concorde: " & Report (E), Warning);
         return Jobs; -- May happen sometimes, don't know why...
   end Evaluate_Concorde;

   -----------------------------
   -- Evaluate_Best_Insertion --
   -----------------------------

   function Evaluate_Best_Insertion (This : in Object;
                                     Jobs : in Sancta.Tasks.Containers.Lists.List;
                                     Job  : in Sancta.Tasks.Object'Class)
                                     return    Sancta.Tasks.Containers.Lists.List
   is
      Result    :          Sancta.Tasks.Containers.Lists.List;
      Best_Cost :          Sancta.Costs := Sancta.Costs'Last;
      Orig_Cost : constant Sancta.Costs := This.Bot.Get_Plan_Cost (Jobs);
      Expand    : constant Sancta.Tasks.Containers.Vectors.Vector :=
                    Anneal.Get_All_Children
                      (Anneal.Parent (Sancta.Tasks.Primitive.Object'Class (Job)));

      procedure Do_Evaluate (Job : in Sancta.Tasks.Object'Class) is
         use Sancta.Tasks.Containers.Lists;
         Eval_List : List := Jobs;
         I         : Cursor := First (Eval_List);
         New_Pos   : Cursor;
      begin
         loop
            declare
               Eval_Cost : Sancta.Costs;
            begin
               Eval_List.Insert (New_Item => Job,
                                 Before   => I,
                                 Position => New_Pos);
               Eval_Cost := This.Bot.Get_Plan_Cost (Eval_List);

               case This.Cost_Policy is
                  when Auctioner.Full_Cost =>
                     if Eval_Cost < Best_Cost then
                        Best_Cost := Eval_Cost;
                        Result    := Eval_List;
                     end if;
                  when Auctioner.Local_Cost =>
                     if Eval_Cost - Orig_Cost < Best_Cost then
                        Best_Cost := Eval_Cost - Orig_Cost;
                        Result    := Eval_List;
                     end if;
               end case;

               Eval_List.Delete (New_Pos);
            end;

            exit when not Has_Element (I);
            Next (I);
         end loop;
      end Do_Evaluate;

   begin
      for I in Expand.First_Index .. Expand.Last_Index loop
         Do_Evaluate (Expand.Element (I));
      end loop;

      if Best_Cost = Sancta.Costs'Last or else Result.Is_Empty then
         raise Cant_Integrate_Task; -- Ummmmmm should transparently manage this
                                 --  but no time
      end if;

      return Result;
   end Evaluate_Best_Insertion;
   --  Say the best point to insert a task (or its alternate)

   -----------------------------
   -- Evaluate_Best_Task_List --
   -----------------------------

   function Evaluate_Best_Task_List (This : in Object;
                                     Jobs : in Sancta.Tasks.Containers.Lists.List;
                                     Job  : in Sancta.Tasks.Object'Class)
                                     return    Sancta.Tasks.Containers.Lists.List
   is
      Full : Sancta.Tasks.Containers.Lists.List := Jobs;
   begin
      Full.Append (Job);
      declare
         L1 : constant Sancta.Tasks.Containers.Lists.List :=
                Evaluate_Best_Plan (This, Full);
         L2 : constant Sancta.Tasks.Containers.Lists.List :=
                Evaluate_Best_Insertion (This, Jobs, Job);
--           L3 : Constant Sancta.Tasks.Containers.Lists.List :=
--                  Evaluate_Concorde (This, Full);
         Best : Sancta.Tasks.Containers.Lists.List;
      begin
         if This.Bot.Get_Plan_Cost (L1) < This.Bot.Get_Plan_Cost (L2) then
            Best := L1;
         else
            Best := L2;
         end if;

         return Best;

--           if This.Bot.Get_Plan_Cost (Best) < This.Bot.Get_Plan_Cost (L3) then
--              return Best;
--           else
--              return L3;
--           end if;
      end;
   end Evaluate_Best_Task_List;

   -----------------------
   -- Evaluate_Removing --
   -----------------------

   function Evaluate_Removing (This : in Object;
                               That : in Sancta.Tasks.Task_Id) return Costs
   is
      use Sancta.Tasks.Containers.Lists;
      Tasks : List   := This.Bot.Get_Tasks;
      I     : Cursor := First (Tasks);
   begin
      while Element (I).Get_Id /= That loop Next (I); end loop;

      Tasks.Delete (I);

      return This.Bot.Get_Plan_Cost (Evaluate_Best_Plan (This, Tasks));
   end Evaluate_Removing;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      Simplebot2.Init (Simplebot2.Object (This));

      This.Subscribe (Network.Groups.Traderbot_Channel);
   end Init;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------
   procedure Process_Incoming_Packet (This    : in out Object;
                                      M       : in     Network.Message'Class;
                                      Meta    : in     Network.Message_Metadata)
   is
   begin
      --  Simplebot things:
      Simplebot2.Object (This).Process_Incoming_Packet (M, Meta);

      if M in Auction_Messages.Offer_Message and then
        (Meta.Receiver = Network.All_Nodes or else Meta.Receiver = This.Link.Id.all)
      then
         declare
            Offer : Auction_Messages.Offer_Message renames Auction_Messages.Offer_Message (M);
         begin
            -- OFFER --
            Log ("Received /OFFER from " & Network.Image (Meta.Sender),
                 Trace.Debug, Section => Log_Section);
            declare
               Job   : Sancta.Tasks.Object'Class renames Offer.Job.Get;
               Prim  : constant Sancta.Tasks.Object'Class :=
                         Anneal.Get_Any_Child (Job);
               --  Prim can possibly be equal to Job, if Job was already primitive.
               Cost  : Costs;
               Full  : Costs;
               Ok    : Boolean := True;
            begin
               if Offer.Must_Bid then
                  This.Dont_Start_Auction_Timer.Reset;
               end if;

               if Offer.Must_Bid or else This.Bidding_Timer.Elapsed >= Auction_Messages.Not_Bidding_Period then
                  This.Bidding_Timer.Reset;
                  --  Compute and send our cost:
                  declare
                     New_Tasks : Sancta.Tasks.Containers.Lists.List;
                  begin
                     begin
                        New_Tasks := Evaluate_Best_Task_List
                          (This,
                           Sancta.Agent.Get_Tasks
                             (Sancta.Agent.Object (This.Bot.all)),
                           Prim);
                     exception
                        when Cant_Integrate_Task =>
                           Ok := False;
                     end;

                     if Ok then
                        Full := This.Bot.Get_Plan_Cost (New_Tasks);
                        Cost := Full -
                          This.Bot.Get_Plan_Cost;

                        if Offer.Cost < 1000000.0 then
                           Log ("Sending BID with cost =" & To_String (Cost) &
                                " (against " & To_String (Offer.Cost) & ")",
                                Trace.Debug, Section => Log_Section);
                        else
                           Log ("Sending BID with cost =" & To_String (Cost),
                                Trace.Debug, Section => Log_Section);
                        end if;
                        if Offer.Full_Cost < 1000000.0 then
                           Log ("Sending BID with full cost =" & To_String (Full) &
                                " (against " & To_String (Offer.Full_Cost) & ")",
                                Trace.Debug, Section => Log_Section);
                        else
                           Log ("Sending BID with full cost =" & To_String (Full),
                                Trace.Debug, Section => Log_Section);
                        end if;
                        This.Link.Send
                          (Meta.Sender, Auction_Messages.Create_Bid_Packet
                             (Job.Get_Id, Cost, Full));
                     end if;
                  end;
               end if;
            end;
         end;
      elsif M in Auction_Messages.Bid_Message then
         -- BID --
         declare
            Bid : Auction_Messages.Bid_Message renames Auction_Messages.Bid_Message (M);
         begin
            if Meta.Receiver = This.Link.Id.all and then This.Auctioning and then
               Bid.Id = This.Task_To_Auction
            then
               Log ("Received bid from " & Network.Image (Meta.Sender) &
                    " with cost" & To_String (Bid.Cost) &
                    " and full cost" & To_String (Bid.Full_Cost),
                    Trace.Debug, Section => Log_Section);
               if Bid.Cost < This.Best_Bid then
                  This.Best_Bid      := Bid.Cost;
                  This.Best_Bidder   := Meta.Sender;
               end if;
               if Bid.Full_Cost < This.Full_Best_Bid then
                  This.Full_Best_Bid      := Bid.Full_Cost;
                  This.Full_Best_Bidder   := Meta.Sender;
               end if;
               This.Bids_Received := This.Bids_Received + 1;
            else
               Log ("Received BID for task not in auction", Trace.Debug, Section => Log_Section);
            end if;
         end;
      elsif M in Auction_Messages.Award_Message then
         -- AWARD --
         declare
            Award : Auction_Messages.Award_Message renames Auction_Messages.Award_Message (M);
         begin
            if Meta.Receiver = This.Link.Id.all then
               Log ("Received /AWARD", Trace.Debug, Section => Log_Section);
               declare
                  Prim : constant Sancta.Tasks.Primitive.Object'Class :=
                           Anneal.Get_Any_Child (Award.Job.Get);
               begin
                  --  Insert in the parent_tasks list if was not primitive:
                  if Prim.Get_Id /= Award.Job.Get.Get_Id then
                     This.Parent_Tasks.Include (Sancta.Tasks.Primitive.Get_Id (Prim),
                                                Sancta.Tasks.Get_Id (Award.Job.Get));
                  end if;
                  --  And add to our job list:
                  This.Add_Task (Prim,
                                 Keep => not This.Auction_Task_Policy.Won);
               end;
            end if;
         end;
      elsif M in Network.Messages.Add_Task_Type then
         -- ADD_TASK --
         declare
            Add_Task : Network.Messages.Add_Task_Type renames
              Network.Messages.Add_Task_Type (M);
         begin
            if Meta.Receiver = This.Link.Id.all then
               Log ("Received ADDTASK", Trace.Debug, Section => Log_Section);
               This.Add_Task (Anneal.Get_Any_Child (Add_Task.Job.Get));
            end if;
         end;
      elsif M in Network.Messages.Shutdown_Type then
         -- SHUTDOWN --
         This.Shutdown := Clock + 1.0; -- Shutdown requested
         This.Bot.Emergency_Stop;
      end if;
   end Process_Incoming_Packet;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done : out Boolean) is
   begin
      --  Parent whereabouts:
      Simplebot2.Run (Simplebot2.Object (This), Done);

      --  Check/End of bids:
      This.Continue_Auction;

      --  Trigger auction if necessary:
      if This.Start_Auction_Timer.Elapsed > This.Auction_Time_Policy.Periodic
        and then This.Dont_Start_Auction_Timer.Elapsed > Auction_Messages.Dont_Start_Auction_Period
      then
         This.Do_Auction;
         This.Start_Auction_Timer.Reset;
      end if;

      --  Stop?
      Done := This.Shutdown < Clock or else Done;
   exception
      when E : others =>
         Log ("Traderbot.Run: " & Report (E), Error);
         raise;
   end Run;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration
     (This                : in out Object;
      Config              : in     Xml.Document;
      Auction_Duration    : in     Duration              := 0.5;
      Auction_Task_Policy : in     Auction_Task_Policies := Default_Auction_Task_Policy;
      Auction_Time_Policy : in     Auction_Time_Policies := Default_Auction_Time_Policy;
      Insertion_Policy    : in     Insertion_Policies    := Best;
      Cost_Policy         : in     Auctioner.Cost_Policies := Auctioner.Full_Cost;
      Max_Bidders         : in     Natural               := Natural'Last)
   is
   begin
      This.Auction_Duration    := Auction_Duration;
      This.Auction_Task_Policy := Auction_Task_Policy;
      This.Auction_Time_Policy := Auction_Time_Policy;
      This.Insertion_Policy    := Insertion_Policy;
      This.Cost_Policy         := Cost_Policy;
      This.Max_Bidders         := Max_Bidders;

      Network.Groups.Init (Config);
   end Set_Configuration;

   -------------------
   -- Task_Finished --
   -------------------

   procedure Task_Finished (This : in out Object;
                            Job  : in out Sancta.Tasks.Primitive.Object'Class)
   is
      use Id_Id_Maps;
      use Sancta.Tasks.Primitive;
   begin
      Log ("Current task finished", Debug, Section => Detail_Section);
      --  Notify parent if exists...
      if Contains (This.Parent_Tasks, Get_Id (Job)) then
         declare
            Pos : constant Cursor := Find (This.Parent_Tasks,
                                           Get_Id (Job));
            Id  : constant Sancta.Tasks.Task_Id := Element (Pos);
         begin
            Log ("Traderbot: Notifiying finished parent:" & Id'Img,
                 Debug, Section => Detail_Section);
            This.Link.Multicast (Network.Groups.Emergency_Channel,
                                 Network.Messages.Task_Done (Id));
         end;
      end if;
      --  Force an auction if policy...
      if This.Auction_Time_Policy.Auction_On_Finalization then
         This.Do_Auction;
      end if;
   end Task_Finished;

end Sancta.Traderbot;
