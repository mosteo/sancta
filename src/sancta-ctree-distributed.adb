with Agpl.Calendar.Format;
with Agpl.Constants;
with Agpl.Conversions;
with Agpl.Drawing.Figures;
with Agpl.Drawing.Predrawer;
--  with Agpl.Drawing.Predrawer.Generic_Color;
with Agpl.Drawing.Predrawer.Stock;
with Agpl.If_Function;
with Agpl.Strings;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Ctree.Tree_Navigator.Partial_Draw;
--  with Sancta.Constants;
with Sancta.Agent_Proxy;
with Sancta.Debug2;
with Sancta.Map.Utils;
with Sancta.Network.Layer.Root;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Positioned;
with Sancta.Tasks.Utils;
with Sancta.Types.Operations;

package body Sancta.Ctree.Distributed is

   package SN renames Sancta.Network;

   use type Agpl.Ustrings.Ustring;
   use type Sancta.Map.Location'Class;
   use type Sancta.Types.Real;

   function Iif is new Agpl.If_Function (String);
   --  function Iif is new Agpl.If_Function (Duration);

   Status_Short : constant array (States) of String (1 .. 2) :=
                    (Pending_Free    => "p_",
                     Pending_Waiting => "pW",
                     Pending_Relay   => "p*",
                     Done_Free       => "d_",
                     Done_Waiting    => "dW",
                     Done_Relay      => "d*");

   Static_Config  : Config_Type;

   ------------
   -- Create --
   ------------

   procedure Create
     (This    : in out Object;
      Config  : Config_Type;
      Base    : Sancta.Node_Id;
      Base_Pose : Sancta.Types.Pose;
      Map     : Sancta.Map.Smart.Object;
      Channel : Sancta.Network.Channel)
   is
      use Agpl.Calendar.Format;
   begin
      This.Subscribe (Channel);

      if This.Link.Id = Base then
         This.Role := Distributed.Base;
      else
         This.Role := Relay; -- Will be fixed during setup phase
      end if;

      This.Channel := Channel;
      This.Config  := Config;
      Static_Config := Config;
      This.Pred.Id := Sancta.No_Node;
      This.Succ.Id := Sancta.No_Node;
      This.Map     := Map;
      This.Base_Id := Base;
      This.Base_Pose := Base_Pose;

      This.Logger_Ctree.Set_File (Name => "ctree." & Image (This.Link.Id) &
                            Datestamp (Separator => '.') & "." &
                                  Timestamp & ".log");

      This.Logger_Signal.Set_File (Name => "signal." & Image (This.Link.Id) &
                            Datestamp (Separator => '.') & "." &
                            Timestamp & ".log");
   end Create;

   ----------------
   -- Get_Config --
   ----------------

   function Get_Config (This : Object) return Config_Type is
   begin
      return This.Config;
   end Get_Config;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config (This : in out Object; Config : Config_Type) is
   begin
      This.Config := Config;
   end Set_Config;

   ----------
   -- Step --
   ----------

   procedure Step
     (This  : in out Object)
   is
   begin
      This.Run; -- Netlistener things
      This.Log_Data;

      if This.Update_Timer.Elapsed >= This.Config.Update_Period then
         This.Update_Timer.Reset;
         This.Send_Update;
         This.Do_Draw;
      end if;

      if This.Mission_Status = Mission_Waiting and then
         This.Setup_Timer.Elapsed > 1.0
      then
         This.Send_Setup;
         This.Setup_Timer.Reset;
      end if;
   end Step;

   -----------------
   -- End_Mission --
   -----------------

   procedure End_Mission (This : in out Object) is
   begin
      Log ("Mission ended", Debug, Log_Section);
      This.Target.Clear;
      This.Backloc.Set (This.Branch.First_Element);
      This.Target.Set
        (Sancta.Tasks.Goto_Pose.Create
           (This.M.Nearest_Pose (This.Backloc.Get)));
      This.Mission_Status := Mission_Completed;
      if This.Role in Head .. Tailhead then
         This.Switch_Status (Done_Waiting);
      end if;
   end End_Mission;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose
     (This : in out Object;
      Pose :        Sancta.Types.Pose)
   is
      use Sancta.Types.Operations;

      --------------------------
      -- Get_Initial_Location --
      --------------------------

      procedure Get_Initial_Location is
      begin
         if not This.Branch.Is_Empty and then not This.Loc.Is_Valid then
            This.Loc.Set (This.M.Nearest_Location_In_Path_Estimated
                          (This.M.Nearest_Location (Pose), This.Branch));
         end if;
      end Get_Initial_Location;

      ---------------------
      -- Check_Next_Step --
      ---------------------

      procedure Check_Next_Step is
         function Prev_Target_Loc return Sancta.Map.Location'Class is
         begin
            return This.Map.Ref.Nearest_Location
              (Sancta.Tasks.Positioned.Object'Class (This.Prev_Target.Get).Pose);
         end Prev_Target_Loc;
      begin
         --  Check proper stepping
         if (not This.Branch.Is_Empty) and then This.Loc.Is_Valid then
            --  Ensure proper location for slaves
            if This.Role in Tail .. Relay and then
              not This.Branch.Contains (This.Loc.Get)
            then
               Log ("LOCATION FIXING at relay", Warning, Log_Section);
               This.Loc := This.Backloc;
               --  On the assumption that we are not that far ... urg.
            end if;

            --  Move forward
            if This.Status = Pending_Free and then
              This.Loc.Get /= This.Branch.Last_Element and then
              Distance (This.Pose, This.M.Nearest_Pose (This.Loc.Get)) <=
              This.Config.Loc_Dist_Threshold
            then
               Log ("Next location", Debug, Log_Section);
               This.Location_Forward;

            elsif This.Status = Done_Free then
               --  Backward, check backpoint
               if This.Role in Head .. Tailhead and then
                 This.Loc.Get = This.Backloc.Get  and then
                 not This.Jobs.Is_Empty
               then
                  declare
                     Next_Branch : constant Sancta.Map.Path :=
                                     This.Navigator.Ref.Branch (This.Target.Get);
                  begin
                     if Next_Branch.Contains (Prev_Target_Loc) or else
                       (This.Succ.Dist >= This.Config.Target_Dist_Threshold * 1.414 and then
                          Sancta.Map.Is_Before (Next_Branch, This.Succ_Loc, This.Loc.Get))
                          --  This last test could be problematic if base is too close
                     then
                        if Next_Branch.Contains (Prev_Target_Loc) then
                           Log ("Backloc: Moving on into same branch",
                                Informative, Log_Section);
                        else
                           Log ("Backloc: Moving on after rigth of way",
                                Informative, Log_Section);
                        end if;
                        This.Switch_Status (Pending_Free);
                        if This.Target.Is_Valid then
                           This.Branch := Next_Branch;
                           This.Branch_Target := This.Target;
                        end if;
                        Log ("Backloc reached", Debug, Log_Section);
                        --  Backward, not yet there
                     else
                        null; -- Wait for a bit of room by succ robot
                     end if;
                  end;

               elsif This.Loc.Get /= This.Branch.First_Element and then
                 Distance (This.Pose, This.M.Nearest_Pose (This.Loc.Get)) <=
                 This.Config.Loc_Dist_Threshold
               then
                  Log ("Prev location", Debug, Log_Section);
                  This.Location_Back;
               end if;
            else
--                 Log ("XXX1 " & This.Status'Img, Always);
--                 Log ("XXX2 " & Boolean'Image (This.Loc.Get = This.Branch.Last_Element), Always);
--                 Log ("XXX3 " & Boolean'Image (Distance (This.Pose, This.M.Nearest_Pose (This.Loc.Get)) <=
--                This.Config.Loc_Dist_Threshold), Always);
               null;
            end if;
         end if;
      end Check_Next_Step;

      --------------------------
      -- Check_Target_Reached --
      --------------------------

      procedure Check_Target_Reached is
      begin
         if (This.Role = Head or else This.Role = Tailhead) and then
           This.Status = Pending_Free and then
           This.Target.Is_Valid and then
           Distance (This.Target_Pose, This.Pose) <
           This.Config.Target_Dist_Threshold
         then
            This.Stats.Mark_Task_Completed;
            This.Jobs_Done.Append (This.Jobs.First_Element);
            This.Prev_Target.Set (This.Jobs.First_Element);
            This.Jobs.Delete_First;
            This.Target.Clear;

            if not This.Jobs.Is_Empty then
               This.Target.Set (This.Jobs.First_Element);
               Log ("Backloc changed", Debug, Log_Section);
               This.Backloc.Set
                 (Sancta.Map.Common_Ancestor
                    (This.Branch,
                     This.Navigator.Ref.Branch (This.Jobs.First_Element)));
               This.Switch_Status (Done_Free);
            else -- Mission ended, retreating.
               This.End_Mission;
            end if;

            pragma Assert (This.Backloc.Is_Valid, "Invalid backloc?");
         end if;
      end Check_Target_Reached;

      ---------------------
      -- Check_Exhausted --
      ---------------------

      procedure Check_Exhausted is
      begin
         if This.Team_Exhausted then
            Log ("TEAM EXHAUSTED, RETREATING!", Warning, Log_Section);
            This.Mission_Status := Mission_Aborted;
            --  This.Jobs.Clear;
            --  Not needed. Also, the Comp will update with the pending ones.
            This.Target.Clear;
            This.Backloc.Set (This.Branch.First_Element);
            This.Target.Set
              (Sancta.Tasks.Goto_Pose.Create
                 (This.M.Nearest_Pose (This.Backloc.Get)));

            This.Loc.Set
              (This.M.Nearest_Location_In_Path_Estimated
                 (This.M.Nearest_Location (This.Pose), This.Branch));
            This.Switch_Status (Done_Free);
         end if;
      end Check_Exhausted;

   begin
      This.Pose := Pose;

      if This.Role /= Base then
         if This.Mission_Status = Mission_Waiting then
            This.Loc.Clear;
            Get_Initial_Location;
         end if;

         Check_Next_Step;

         if This.Role in Head .. Tailhead then
            Check_Target_Reached;
         end if;

         This.Check_Links;
         This.Check_Links;
         --  Twice because this can save one step in the automaton.
      end if;

      if This.Mission_Status > Mission_Waiting and then
        This.Role in Head .. Tailhead
      then
         Check_Exhausted;
      end if;

      if This.Role /= Base then
         This.Report;
      end if;
   end Set_Pose;

   -------------------
   -- Set_Qualities --
   -------------------

   procedure Set_Qualities (This : in out Object;
                            Q    :        Id_Q_Maps.Map)
   is
      use Agpl.Conversions;
      use Id_Q_Maps;
      procedure Set_Links_Print (I : Cursor) is
      begin
         Log (Sancta.Image (Key (I)) & ": " & To_String (Float (Element (I))),
              Debug, Det_Section);
      end Set_Links_Print;
   begin
      This.Qs := Q; -- Store full quality row
      This.Log_Signal;

      if Q.Contains (This.Pred.Id) then
         This.Pred.Q := Q.Element (This.Pred.Id);
         Log ("Setting Pred.Q to " & This.Pred.Q'Img, Debug, Log_Section);
      end if;
      if Q.Contains (This.Succ.Id) then
         This.Succ.Q := Q.Element (This.Succ.Id);
            Log ("Setting Succ.Q to " & This.Succ.Q'Img, Debug, Log_Section);
      end if;

      Log ("Qs: " & Q.Length'Img, Debug, Det_Section);
      Q.Iterate (Set_Links_Print'Access);
   end Set_Qualities;

   -------------------
   -- Switch_Status --
   -------------------

   procedure Switch_Status (This : in out Object; New_Status : States) is
         package Pad is new Agpl.Strings.Enum_Padding (States);
      begin
         Log ("Status: " & Pad.Pad (This.Status) & " --> " & Pad.Pad (New_Status),
              Informative, Log_Section);
         This.Status := New_Status;
      end Switch_Status;

   -----------------
   -- Check_Links --
   -----------------

   procedure Check_Links (This : in out Object) is
   begin
      case This.Status is

         when Pending_Free =>
            This.Relay_Timer.Reset;
            --  Stop by signal
            if This.Succ.Q < This.Config.Signal_Threshold then
               This.Switch_Status (Pending_Relay);
            elsif
              --  Or butt reaching
              (This.Role in Tail .. Relay and then
                 This.Pred.Dist <= This.Config.Here_Dist_Threshold) or else
            --  Or leaving behind a non-relay
              (This.Role in Relay .. Head and then
                 This.Succ.Status in Pending_Waiting .. Pending_Free and then
                   This.Succ.Dist >= This.Config.Near_Dist_Threshold)
            then
               This.Switch_Status (Pending_Waiting);
            end if;

         when Pending_Relay =>
            --  Move again by signal
            if This.Succ.Q >= This.Config.Signal_Threshold and then
              This.Relay_Timer.Elapsed < This.Config.Fixed_Relay_Period
            then
               This.Switch_Status (Pending_Waiting);
            end if;
         when Pending_Waiting =>
            This.Relay_Timer.Reset;
            --  Space ahead
            if (This.Role in Head .. Tailhead or else
                 This.Pred.Dist >= This.Config.Here_Dist_Threshold) and then
            --  Not leaving behind
              (This.Role = Tail or else This.Role = Tailhead or else
                 This.Succ.Status = Pending_Relay or else
                 This.Succ.Dist <= This.Config.Near_Dist_Threshold)
            then
               This.Switch_Status (Pending_Free);
            end if;

         when Done_Free =>
            This.Relay_Timer.Reset;
            --  Stop by signal?
            if (This.Pred.Id /= No_Node and then
                  This.Pred.Q < This.Config.Signal_Threshold)
            then
               This.Switch_Status (Done_Relay);
            elsif
              --  Or butt reaching
              (This.Role in Relay .. Head and then
                 This.Succ.Dist <= This.Config.Here_Dist_Threshold) or else
            --  Or leaving behind
              (This.Role in Tail .. Relay and then
                 This.Pred.Dist >= This.Config.Near_Dist_Threshold)
            then
               This.Switch_Status (Done_Waiting);
            end if;

         when Done_Relay =>
            --  Move again by signal
            if (This.Pred.Id = No_Node or else
                  This.Pred.Q >= This.Config.Signal_Threshold)
            then
               This.Switch_Status (Done_Waiting);
            end if;
         when Done_Waiting =>
            This.Relay_Timer.Reset;
            --  Space ahead
            if (This.Role = Tail or else This.Role = Tailhead or else
                  (This.Succ.Parked and then
                   This.Succ.Dist >= This.Config.Loc_Dist_Threshold) or else
                This.Succ.Dist >= This.Config.Here_Dist_Threshold) and then
            --  Not leaving behind
              (This.Role in Head .. Tailhead or else
                 This.Pred.Dist <= This.Config.Near_Dist_Threshold)
            then
               This.Switch_Status (Done_Free);
            end if;
      end case;
   end Check_Links;

   -----------------------
   -- Set_Ordered_Tasks --
   -----------------------

   procedure Set_Ordered_Tasks
     (This : in out Object;
      Jobs :        Tc.Lists.List)
   is
      use Sancta.Tasks.Utils;
   begin
      This.Jobs := Jobs - This.Jobs_Done;
      if This.Branch.Is_Empty and then
        This.Navigator.Is_Valid and then
        not Jobs.Is_Empty
      then
         This.Branch        := This.Navigator.Ref.Branch (Jobs.First_Element);
         This.Branch_Target.Set (Jobs.First_Element);
         if not Jobs.Is_Empty then
            This.Target.Set (Jobs.First_Element);
         end if;
      end if;

      Log ("Jobs: " & Jobs.Length'Img, Debug, Det_Section);
   end Set_Ordered_Tasks;

   -------------------
   -- Set_Navigator --
   -------------------

   procedure Set_Navigator
     (This : in out Object;
      Nav  :        Sancta.Ctree.Tree_Navigator.Handle.Object)
   is
   begin
      This.Navigator := Nav;
   end Set_Navigator;

   ------------
   -- Status --
   ------------

   function Status
     (This : Object)
      return Mission_States
   is
      use Sancta.Types.Operations;
   begin
      return This.Mission_Status;
   end Status;

   -----------------
   -- Action_Park --
   -----------------

   function Action_Park (This : Object) return Robot_Orders is
      use type Types.Angle;
   begin
      if abs (This.Pose.A - This.Operator_Msg.Get.Towards) < 0.2 then
         return (Action => Wait);
      else
         return This.Action_Turn (Towards => This.Operator_Msg.Get.Towards);
      end if;
   end Action_Park;

   -----------------
   -- Action_Turn --
   -----------------

   function Action_Turn (This    : Object;
                         Towards : Types.Angle)
                         return     Robot_Orders
   is
      use Sancta.Types;
      Off  : constant Types.Angle := Towards - This.Pose.A;
      Sign : constant Types.Angle := Off / abs Off;
   begin
      return (Action => Turn,
              Velo   => (0.0, 0.0, This.Config.Rot_Vel * Sign));
   end Action_Turn;

   -----------------
   -- Action_Turn --
   -----------------

   function Action_Turn (This    : Object;
                         Towards : Types.Pose)
                         return     Robot_Orders
   is
      use Sancta.Types;
      use Sancta.Types.Operations;
      Off  : constant Types.Angle := Offset (This.Pose, Towards);
      Sign : constant Types.Angle := Off / abs Off;
   begin
      return (Action => Turn,
              Velo   => (0.0, 0.0, This.Config.Rot_Vel * Sign));
   end Action_Turn;

   ------------
   -- Orders --
   ------------

   function Orders
     (This : Object)
      return Robot_Orders
   is
      use Sancta.Types.Operations;
      use type Sancta.Types.Angle;
   begin
      case This.Operator_Msg.Get.Action is
         when Stop =>
            return (Action => Wait);
         when Park =>
            return This.Action_Park;
         when Go =>
            null;
            --  This is the only pass-through, for regular processing...
         when others =>
            raise Program_Error
              with "Unexpected action: " & This.Operator_Msg.Get.Action'Img;
      end case;

      if not This.Branch.Is_Empty and then This.Loc.Is_Valid then
         if Moving_States (This.Status) and then
           not (This.Loc.Get = This.M.Nearest_Location (This.Target_Pose)) and then
           abs Offset (This.Pose, This.Loc_Pose) > 0.8
         then
            return This.Action_Turn (Towards => This.Loc_Pose);
         else
            case This.Status is

               when Pending_Free =>
                  if This.Loc.Get = This.Branch.Last_Element then
                     return
                       (Action => Move,
                        Goal   => This.Target_Pose);
                  elsif This.Loc.Get = This.M.Nearest_Location (This.Target_Pose) then
                     --  Seems to be the same as the previous one, since the last
                     --  branch location in the target location!!
                     return
                       (Action => Move,
                        Goal   => This.Target_Pose);
                  else
                     return
                       (Action => Move,
                        Goal   => This.M.Nearest_Pose (This.Loc.Get));
                  end if;

               when Done_Free =>
                  if This.Loc.Get = This.Branch.First_Element and then
                    Distance (This.Pose, This.M.Nearest_Pose (This.Loc.Get)) <=
                    This.Config.Target_Dist_Threshold
                  then
                     return (Action => Wait);
                  else
                     return
                       (Action => Move,
                        Goal   => This.M.Nearest_Pose (This.Loc.Get));
                  end if;

               when others =>
                  return (Action => Wait);
            end case;
         end if;
      else
         return (Action => Wait);
      end if;
   end Orders;

   -------------------
   -- Pending_Tasks --
   -------------------

   function Pending_Tasks
     (This : Object)
      return Tc.Lists.List
   is
   begin
      return This.Jobs;
   end Pending_Tasks;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet
     (This : in out Object;
      M    : in     Network.Message'Class;
      Meta : in     Network.Message_Metadata)
   is
   begin
      if Meta.Sender /= This.Link.Id then
         This.Silence_Timer.Reset;
      end if;

      if M in Ctree_Msg'Class then
         Ctree_Msg'Class (M).Process (Meta, This);
      end if;
   end Process_Incoming_Packet;

   ------------
   -- Report --
   ------------

   procedure Report (This : Object) is
      use Agpl.Conversions;
      use Sancta.Types.Operations;
   begin
      Log ("********************************", Debug, Log_Section);
      Log ("Stat: " & This.Status'Img, Debug, Log_Section);
      if not This.Branch.Is_Empty then
         Log ("Park: " & Boolean'Image
           (Distance (This.Pose,
              This.M.Nearest_Pose (This.Branch.First_Element)) <=
                This.Config.Parking_Range),
           Debug, Log_Section);
      end if;
      Log ("Pose: " & Debug2.To_String (This.Pose), Debug, Log_Section);
      Log ("Base: " & Debug2.To_String (This.Base_Pose),
           Debug, Log_Section);
      if This.Loc.Is_Valid then
         Log ("Loc : " & This.Loc.Get.Image, Debug, Log_Section);
         Log ("Dpth:" & This.Loc.Ref.Depth (This.Branch)'Img,
              Debug, Log_Section);
      else
         Log ("Loc : INVALID", Debug, Log_Section);
      end if;
      if This.Loc.Is_Valid and then This.Target.Is_Valid then
         Log ("On Target: " & Boolean'Image
              (This.Loc.Get = This.M.Nearest_Location (This.Target_Pose)),
              Debug, Log_Section);
      end if;
      Log ("Brlen:" & This.Branch.Length'Img, Debug, Log_Section);
      if This.Backloc.Is_Valid then
         Log ("B-loc: " & This.Backloc.Ref.Image, Debug, Log_Section);
      else
         Log ("B-loc: NONE", Debug, Log_Section);
      end if;
      Log ("pred-Q: " & To_String (Float (This.Pred.Q)), Debug, Log_Section);
      Log ("succ-Q: " & To_String (Float (This.Succ.Q)), Debug, Log_Section);
      Log ("succId: " & Image (This.Succ.Id), Debug, Log_Section);
      Log ("Orders: " & Image (This.Orders), Debug, Log_Section);
      if This.Loc.Is_Valid then
         Log ("Goal D: " & To_String
              (Float (Distance (This.Pose, This.M.Nearest_Pose (This.Loc.Get)))),
              Debug, Log_Section);
      end if;
      if This.Target.Is_Valid then
         Log ("Trgt D: " & To_String
              (Float (Distance (This.Pose, This.Target_Pose))),
              Debug, Log_Section);
      end if;
      Log ("Jobs:" & This.Jobs.Length'Img, Debug, Log_Section);
      if This.Jobs.Is_Empty then
         Log ("Curr: NONE", Debug, Log_Section);
      else
         Log ("Curr: " & This.Jobs.First_Element.To_String,
              Debug, Log_Section);
      end if;
      if This.Role in Tail .. Relay then
         Log ("Pred D: " & This.Rel_Dist (This.Pred.Dist)'Img &
              " " & To_String (Float (This.Pred.Dist)),
              Debug, Log_Section);
      end if;
      if This.Role in Tail .. Head then
         Log ("Succ D: " & This.Rel_Dist (This.Succ.Dist)'Img &
              " " & To_String (Float (This.Succ.Dist)),
              Debug, Log_Section);
      end if;
   end Report;

   ----------------------
   -- One_Liner_Report --
   ----------------------

   function One_Liner_Report (This : Object) return String is
   begin
      return
        Status_Short (This.Status) & ":" &
      Iif (This.Parked, "P", "_");
   end One_Liner_Report;

   ------------
   -- Report --
   ------------

--     function Report (Msg  : Msg_Global_Update) return String
--     is
--        use Agpl.Strings;
--     begin
--        return
--          Status_Short (Msg.Status) & ":" &
--        Iif (Msg.Parked, "P", "") & ":" &
--        Iif (Msg.Relay_Time >= 1.0,
--             To_String (Float (Msg.Relay_Time), 1),
--             "") & ":" &
--        Iif (Msg.Exhaust_Time >= 1.0,
--             To_String (Float (Msg.Exhaust_Time), 1),
--             "");
--     end Report;

   -----------------
   -- Send_Update --
   -----------------

   procedure Send_Update (This : in out Object) is
      use Sancta.Network;
      use Sancta.Types.Operations;
   begin
      This.Msg_Seq := This.Msg_Seq + 1;

      if (This.Role = Head or else This.Role = Tailhead) then
         if This.Succ.Id /= No_Node then
            --  Send the current targets...
            This.Link.Send
              (New_Address (This.Succ.Id, This.Channel),
               Msg_Mission_Update'(Prev_Target   => This.Prev_Target,
                                   Curr_Target   => This.Target,
                                   Backloc       => This.Backloc,
                                   Branch_Target => This.Branch_Target,
                                   Mission_Done  =>
                                     This.Mission_Status in
                                       Mission_Completed .. Mission_Aborted));

            --  Send heartbeat...
            This.Link.Send
              (New_Address (This.Succ.Id, This.Channel),
               Msg_Heartbeat'(Seq => This.Msg_Seq));
         end if;

         --  Send the global information seed message
         declare
            Target_Loc : Map.Location_Handle.Object;
            Info       : Id_Info_Maps.Map;
         begin
            if This.Target.Is_Valid then
               Target_Loc.Set (This.M.Nearest_Location (This.Target_Pose));
            end if;
            Info.Include -- This is sent to self to store it in head
              (This.Link.Id,
               Robot_Info'(From       => This.Link.Id,
                           Succ       => This.Succ.Id,
                           Role       => This.Role,
                           Status     => This.Status,
                           Parked     => This.Parked,
                           Pose       => This.Pose,
                           Succ_Pose  => This.Succ.Pose,
                           Pred_Dist  => This.Pred.Dist,
                           Q          => This.Succ.Q,
                           Loc        => This.Loc,
                           Relay_Time => This.Relay_Timer.Elapsed));
            This.Link.Send
              (New_Address (This.Link.Id, This.Channel),
               Msg_Head_To_Tail_Global_Status'(Info, Target_Loc));
         end;
      end if;

      --  Send the reverse report
      if This.Role = Base and then This.Pred.Id /= No_Node then
         This.Log_Time := This.Mission_Timer.Elapsed;
         This.Log_Delta.Reset;
         This.Link.Send
           (New_Address (This.Pred.Id, This.Channel),
            Msg_Tail_To_Head_Status'(Exhausted => True,
                                     Log_Time  => This.Log_Time));
      end if;

      --  Send the local neighborhood report
      declare
         Msg : constant Msg_Robot_Local_Update :=
                 (Message with
                  Seq     => This.Msg_Seq,
                  From    => This.Link.Id,
                  Pose    => This.Pose,
                  Status  => This.Status,
                  Parked  => This.Parked);
      begin
         --  Just for the benefit of the signal_distance component:
         This.Link.Send (New_Address (This.Link.Id, This.Channel), Msg);

         --  These are really necessary:
         if This.Succ.Id /= No_Node and then
           (This.Role in Tail .. Head)
         then
            This.Link.Send (New_Address (This.Succ.Id, This.Channel), Msg);
         end if;
         if This.Pred.Id /= No_Node then
            This.Link.Send (New_Address (This.Pred.Id, This.Channel), Msg);
         end if;
      end;

      --  Send the global robot report
      --  Both messages pass by the own robot, bit of waste but we avoid checks.
      This.Link.Send (New_Address (This.Link.Id, This.Channel),
        Msg_Robot_Global_Update'(Way  => To_Head,
                                 Id   => This.Link.Id,
                                 Pose => This.Pose));
      This.Link.Send (New_Address (This.Link.Id, This.Channel),
        Msg_Robot_Global_Update'(Way  => To_Tail,
                                 Id   => This.Link.Id,
                                 Pose => This.Pose));
   end Send_Update;

   ----------------
   -- Send_Setup --
   ----------------

   procedure Send_Setup (This : in out Object) is
      function Closer (L, R : Robot_Info) return Boolean is
         use Types.Operations;
      begin
         return
           Distance (L.Pose, This.Pose) < Distance (R.Pose, This.Pose) or else
           (Distance (L.Pose, This.Pose) = Distance (R.Pose, This.Pose) and then
            L.From < R.From);
      end Closer;

      package Sort is new Info_Vectors.Generic_Sorting (Closer);
   begin
      if This.Role = Base then
         Sort.Sort (This.Setup_Info);
         if not This.Setup_Info.Is_Empty then
            if This.Pred.Id /= This.Setup_Info.First_Element.From then
               Log ("Base.Pred is " & (-This.Pred.Id), Informative, Log_Section);
               This.Pred.Id := This.Setup_Info.First_Element.From;
            end if;
         end if;
         declare
            Msg : Msg_Setup_Reply;
         begin
            for I in This.Setup_Info.First_Index .. This.Setup_Info.Last_Index loop
               Msg.Tail_To_Head.Append (This.Setup_Info.Element (I).From);
            end loop;
            This.Link.Send (This.Channel, Msg);
         end;
      else
         This.Link.Send
           (SN.New_Address (This.Base_Id, This.Channel),
            Msg_Setup_Request'(Pose => This.Pose));
      end if;
   end Send_Setup;

------------
-- Parked --
------------

   function Parked (This : Object) return Boolean is
      use Sancta.Types.Operations;
   begin
      if This.Branch.Is_Empty then
         return True;
      else
         return
           Distance
             (This.Pose, This.M.Nearest_Pose (This.Branch.First_Element))
           <=
           This.Config.Parking_Range;
      end if;
   end Parked;

   -------
   -- M --
   -------

   function M (This : Object) return Sancta.Map.Object_Access is
   begin
      return This.Map.Ref;
   end M;

   -----------
   -- Image --
   -----------

   function Image (Orders : Robot_Orders) return String is
      S : Ustring := + Orders.Action'Img;
   begin
      if Orders.Action = Move then
         Asu.Append (S, " Goal: " & Debug2.To_String (Orders.Goal));
      end if;

      return +S;
   end Image;

   ------------
   -- Report --
   ------------

   function Report (Info : Robot_Info) return String
   is
      use Agpl.Strings;
   begin
      return
        Capitalize (Info.Role'Img) & ":" &
        Status_Short (Info.Status) & ":" &
        Iif (Info.Parked, "P", "") & ":" &
        Iif (Info.Relay_Time >= 1.0,
             To_String (Float (Info.Relay_Time), 1),
             "");
   end Report;

   ----------
   -- Pose --
   ----------

   function Pose   (X : Node_Status) return Sancta.Types.Pose is
   begin
      return X.Pose;
   end Pose;

   -------
   -- Q --
   -------

   function Q      (X : Node_Status) return Sancta.Network.Qualities.Quality is
   begin
      return X.Succ_Q;
   end Q;

   ---------------
   -- Threshold --
   ---------------

   function Threshold (X : Node_Status) return Sancta.Network.Qualities.Quality is
   begin
      return X.Threshold;
   end Threshold;

   ------------
   -- Report --
   ------------

   function Report (X : Node_Status) return String is
   begin
      return +X.Report;
   end Report;

   -------------
   -- Do_Draw --
   -------------

   procedure Do_Draw (This : in out Object) is
      use type Agpl.Drawing.Multisource.Object_Access;
   begin
      if This.Draw = null then
         return;
      end if;
      This.Draw.Draw
        ("ctree_full",
         This.Get_Draw_Status);
   end Do_Draw;

   ---------------------
   -- Get_Draw_Status --
   ---------------------

   function Get_Draw_Status (This : Object) return Status_Draw'Class is
   begin
      return Status_Draw'
        (Base_Pose  => This.Base_Pose,
         Navigator  => This.Navigator,
         Jobs_Pend  => This.Jobs,
         Jobs_Done  => This.Jobs_Done,
         Global     => This.Last_Global_Status);
   end Get_Draw_Status;

   ---------------------
   -- Get_Node_Status --
   ---------------------

   function Get_Node_Status (This : Object) return Node_Status'Class is
   begin
      if This.Last_Global_Status.Info.Contains (This.Link.Id) then
         return Node_Status'
           (Role      => This.Role,
            Pose      => This.Pose,
            Succ_Q    => This.Succ.Q,
            Threshold => This.Config.Signal_Threshold,
            Status    => This.Status,
            Report    => +Report (This.Last_Global_Status.Info.Element (This.Link.Id)));
      elsif This.Role = Base then
         return Node_Status'
           (Role      => This.Role,
            Pose      => This.Pose,
            Succ_Q    => This.Succ.Q,
            Threshold => This.Config.Signal_Threshold,
            Status    => This.Status,
            Report    => +"Head lost msgs:" & This.N_Hop_Lost_Msgs'Img);
      else
         return Node_Status'
           (Role      => This.Role,
            Pose      => This.Pose,
            Succ_Q    => This.Succ.Q,
            Threshold => This.Config.Signal_Threshold,
            Status    => This.Status,
            Report    => +"Not linked yet");
      end if;
   end Get_Node_Status;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : Status_Draw; Into : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Drawing;
      use Agpl.Drawing.Predrawer.Stock;
      use Tasks.Utils;
   begin
      --  Draw targets
      if Ctree_Status_Draw_Targets.Value then
         Into.Set_Color (Agpl.Constants.Black, 0);
         declare
            procedure Draw (I : Tc.Lists.Cursor) is
               Job : constant Sancta.Tasks.Object'Class := Tc.Lists.Element (I);
            begin
               if Job in Agpl.Drawing.Drawable'Class then
                  Agpl.Drawing.Drawable'Class (Job).Draw (Into);
               end if;
            end Draw;
         begin
            This.Jobs_Pend.Iterate (Draw'Access);
         end;
      end if;

      --  Draw tree
      if Ctree_Status_Draw_Tree.Value and then This.Navigator.Is_Valid then
         declare
            Backtasks : Tc.Lists.List := This.Jobs_Done;
            --  Tasks done plus first undone.
            --  Trick: calculate backpoint from one task to the next
            --  Draw only the suffix of backpoint to first task path
         begin
            Into.Set_Color (Agpl.Constants.Gray, Agpl.Constants.Alpha_Opaque);

            if not This.Jobs_Pend.Is_Empty then
               Backtasks.Append (This.Jobs_Pend.First_Element);
               declare
                  I : Tc.Lists.Cursor := Backtasks.First;
               begin
                  while Tc.Lists.Has_Element (I) and then
                  Tc.Lists.Has_Element (Tc.Lists.Next (I))
                  loop
                     declare
                        B1     : constant Sancta.Map.Path :=
                                   This.Navigator.Ref.Branch (Tc.Lists.Element (I));
                        B2     : constant Sancta.Map.Path :=
                                   This.Navigator.Ref.Branch (Tc.Lists.Element (Tc.Lists.Next (I)));
                        Back   : constant Sancta.Map.Location'Class :=
                                   Sancta.Map.Common_Ancestor (B1, B2);
                        Suffix : Sancta.Map.Path := Sancta.Map.Tail (B1, Back);
                     begin
                        Suffix.Prepend (Back);
                        Sancta.Map.Utils.Draw_Path (Suffix, Into);
                        Tc.Lists.Next (I);
                     end;
                  end loop;
               end;

               --  Draw pending in Navy:
               Into.Set_Color (Agpl.Constants.Navy, Agpl.Constants.Alpha_Opaque);
               Tree_Navigator.Partial_Draw.Create
                 (This.Navigator.Ref, This.Jobs_Pend).Draw (Into);
            else
               --  Nothing pending, draw all as done:
               Tree_Navigator.Partial_Draw.Create
                 (This.Navigator.Ref, This.Jobs_Done).Draw (Into);
            end if;
         end;
      end if;

      --  Draw global info from all robots
      declare
         Idx : Positive := 1;
         use Id_Info_Maps;
         procedure Draw_Info (I : Cursor) is
            Info : constant Robot_Info := Element (I);
         begin
            if Info.Loc.Is_Valid and then
              Info.Loc.Ref.all in Agpl.Drawing.Drawable'Class
            then
               declare
                  Red_Loc : Predrawer.Object
                    (Color_Red'Access, Drawable_Access (Info.Loc.Ref));
                  Q       : constant Agpl.Drawing.Figures.Vprogress :=
                              (Float (Info.Pose.X),
                               Float (Info.Pose.Y + 2.0),
                               0.4,
                               3.0,
                               Float (Signal_Q'Last),
                               Float (Info.Q),
                               Text_Above => True,
                               Show_Pct   => False);
                  pragma Warnings (Off);
                  Txt     : aliased constant Drawable'Class :=
                              Agpl.Drawing.Figures.Text
                                (Float (Info.Pose.X), Float (Info.Pose.Y) + 1.5,
                                 Report (Info));
                  pragma Warnings (On);
                  Black_Txt : Predrawer.Object
                    (Color_Black'Access, Txt'Access);

                  subtype F is Float;
               begin
                  if Ctree_Status_Draw_Locations.Value then
                     Red_Loc.Draw (Into);
                  end if;

                  if Ctree_Status_Draw_Signal.Value then
                     Q.Draw (Into);
                  end if;

                  if Ctree_Status_Draw_Links.Value then
                     if Relay_States (Info.Status) then
                        Into.Set_Color (Agpl.Constants.Red,
                                        Agpl.Constants.Alpha_Opaque);
                     elsif Info.Status = Done_Waiting and then
                       Info.Pred_Dist >= Static_Config.Near_Dist_Threshold + 1.0
                     then
                        Into.Set_Color (Agpl.Constants.Red,
                                        Agpl.Constants.Alpha_Opaque);
                     else
                        Into.Set_Color (Agpl.Constants.Green,
                                        Agpl.Constants.Alpha_Opaque);
                     end if;
                     Into.Draw_Line (F (Info.Pose.X),
                                     F (Info.Pose.Y),
                                     F (Info.Succ_Pose.X),
                                     F (Info.Succ_Pose.Y));
                  end if;

                  Black_Txt.Draw (Into);
               end;
            end if;
            Idx := Idx + 1;
         end Draw_Info;
      begin
         This.Global.Info.Iterate (Draw_Info'Access);
      end;

      --  Draw target location
      if Ctree_Status_Draw_Targets.Value and This.Global.Target_Loc.Is_Valid then
         declare
            Helper_Loc : constant Sancta.Map.Location_Handle.Object :=
                           This.Global.Target_Loc;
         begin
            if This.Global.Target_Loc.Get in Drawable'Class then
               declare
                  Green_Loc  : Predrawer.Object
                    (Color_Dark_Green'Access,
                     Drawable_Access (Helper_Loc.Ref));
               begin
                  Green_Loc.Draw (Into);
--                    This.Draw.Draw ("target_loc", Green_Loc);
               end;
            end if;
         end;
      else
         null;
--           This.Draw.Draw ("target_loc", Figures.Null_Figure);
      end if;

   exception
      when E : others =>
         Log ("Status_Draw.Draw: " & Report (E), Error, Log_Section);
         raise;
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This : Node_Status; Into : in out Agpl.Drawing.Drawer'Class)
   is
   begin
      if This.Role /= Base then
         Into.Set_Color ((255, 0, 0), 0);
         Agpl.Drawing.Figures.Robot
           (Float (This.Pose.X),
            Float (This.Pose.Y),
            Float (This.Pose.A)).Draw (Into);
      else
         Into.Set_Color ((0, 0, 0), 0);
         Into.Draw_Rectangle (-1.0, -1.0, 1.0, 1.0);
         Into.Draw_Line (-1.0, -1.0, 1.0, 1.0);
         Into.Draw_Line (-1.0, 1.0, 1.0, -1.0);
      end if;
   end Draw;

   ----------------
   -- Set_Drawer --
   ----------------

   procedure Set_Drawer
     (This : in out Object;
      Draw :        Agpl.Drawing.Multisource.object_access)
   is
   begin
      This.Draw := Draw;
   end Set_Drawer;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Mission_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is
      pragma Unreferenced (From);
      use type Sancta.Map.Path;
      use Sancta.Network;
      use Sancta.Tasks;
      use Sancta.Tasks.Utils;
   begin
      if This.Role not in Head .. Tailhead then
         if This.Target.Ref.all /= Msg.Curr_Target.Ref.all then
            Log ("Switching backloc due to target update", Debug, Log_Section);
            This.Prev_Target := Msg.Prev_Target;
            This.Target      := Msg.Curr_Target;
            This.Backloc     := Msg.Backloc;
            if Msg.Prev_Target.Is_Valid then
               This.Jobs_Done.Append (Msg.Prev_Target.Get);
            end if;
         end if;

         if This.Mission_Status = Mission_Snafu and then
           not Msg.Mission_Done
         then
            if This.Branch_Target.Is_Valid and then
              This.Branch /= This.Navigator.Ref.all.Branch (Msg.Branch_Target.Ref.all)
            then
               Log ("Switching branch due to backloc reached", Debug, Log_Section);
               Log ("CURR: " & Msg.Curr_Target.Get.To_String, Always);
               This.Branch := This.Navigator.Ref.all.Branch (Msg.Branch_Target.Get);
            end if;
         end if;
      end if;

      This.Last_Mission_Update := Msg;

      if This.Succ.Id /= No_Node then
         This.Link.Send (New_Address (This.Succ.Id, This.Channel), Msg);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Robot_Local_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is
      pragma Unreferenced (From);
      use Sancta.Map;
   begin
      Update_Neighbor (This.Pred, Msg);
      Update_Neighbor (This.Succ, Msg);

      --  One-hop checking
      if Msg.From = This.Pred.Id then
         --  Ordered delivery is needed! (RTWMP complies)
         if This.Mission_Status /= Mission_Waiting then
            This.One_Hop_Lost_Msgs   := This.One_Hop_Lost_Msgs + Msg.Seq -
              This.One_Hop_Last_In_Seq - 1;
            This.One_Hop_Msg_Count   := This.One_Hop_Msg_Count + Msg.Seq -
              This.One_Hop_Last_In_Seq;
         end if;
         This.One_Hop_Last_In_Seq := Msg.Seq;
      end if;

      --  Change of status based on task completion
      if This.Role in Tail .. Relay and then
        This.Status in Pending_Relay .. Pending_Free and then
        This.Pred.Status in Done_Relay .. Done_Free
      then
         Log ("Switching from PENDING to DONE", Debug, Log_Section);
         case This.Status is
            when Pending_Relay =>
               This.Switch_Status (Done_Relay);
            when others =>
               This.Switch_Status (Done_Waiting);
         end case;

         This.Loc.Set
           (This.M.Nearest_Location_In_Path_Estimated
              (This.M.Nearest_Location (This.Pose), This.Branch));
      end if;

      if This.Role in Tail .. Relay and then
        This.Status in Done_Relay .. Done_Free and then
        This.Pred.Status in Pending_Relay .. Pending_Free
      then
         Log ("Switching from DONE to PENDING", Debug, Log_Section);
         case This.Status is
            when Done_Relay =>
               This.Switch_Status (Pending_Relay);
            when others =>
               This.Switch_Status (Pending_Waiting);
               This.Relay_Timer.Reset;
         end case;

         This.Loc.Set
           (This.M.Nearest_Location_In_Path_Estimated
              (This.M.Nearest_Location (This.Pose), This.Branch));
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Robot_Global_Update;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class)
   is
      pragma Unreferenced (From);
      use Sancta.Network;
   begin
      if Msg.Way = To_Head and then not This.Is_Head and then This.Pred.Id /= No_Node then
         This.Link.Send (New_Address (This.Pred.Id, This.Channel), Msg);
      end if;

      if Msg.Way = To_Tail and then not (This.Role = Base) and then This.Succ.Id /= No_Node then
         This.Link.Send (New_Address (This.Succ.Id, This.Channel), Msg);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Operator_Control;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is
      pragma Unreferenced (From);
   begin
      case Msg.Action is
         when Go | Park | Cancel =>
            This.Commanding_Robot := True;
         when Stop =>
            This.Commanding_Robot := False;
      end case;

      if Msg.Action /= Cancel then
         This.Operator_Msg.Set (Msg);
      end if;

      Log ("Operator command: " & Msg.Action'Img, Informative, Log_Section);
      case Msg.Action is
         when Go =>
            This.Relay_Timer.Reset;
            if not (This.Mission_Status = Mission_Snafu)  then
               Log ("ROBOTS RELEASED, MISSION STARTED", Always, Log_Section);
               This.Mission_Status := Mission_Snafu;
               This.Mission_Timer.Reset;
               This.Relay_Timer.Reset;
               This.Exhaust_Timer.Reset;
               This.Stats.Init (This.Dummy_Links);
               This.One_Hop_Msg_Count := 0;
               This.One_Hop_Lost_Msgs := 0;
               This.N_Hop_Msg_Count   := 0;
               This.N_Hop_Lost_Msgs   := 0;
            end if;
         when Cancel =>
            Log ("Mission cancelled, going back...", Informative, Log_Section);
            This.End_Mission;
         when others =>
            null;
      end case;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Head_To_Tail_Global_Status;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is
      pragma Unreferenced (From);
      use Sancta.Network;
   begin
      This.Last_Global_Status := Msg;
      if This.Succ.Id /= No_Node then
         This.Last_Global_Status.Info.Include -- It can be the head robot
           (This.Link.Id,
            Robot_Info'(From       => This.Link.Id,
                        Succ       => This.Succ.Id,
                        Role       => This.Role,
                        Status     => This.Status,
                        Parked     => This.Parked,
                        Pose       => This.Pose,
                        Succ_Pose  => This.Succ.Pose,
                        Pred_Dist  => This.Pred.Dist,
                        Q          => This.Succ.Q,
                        Loc        => This.Loc,
                        Relay_Time => This.Relay_Timer.Elapsed));
         This.Link.Send (New_Address (This.Succ.Id, This.Channel),
                         This.Last_Global_Status);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Heartbeat;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class) is
      pragma Unreferenced (From);
      use Sancta.Network;
   begin
      --  Ordered delivery is needed! (RTWMP complies)
      This.N_Hop_Lost_Msgs   := This.N_Hop_Lost_Msgs + Msg.Seq -
                                This.N_Hop_Last_In_Seq - 1;
      This.N_Hop_Msg_Count   := This.N_Hop_Msg_Count + Msg.Seq -
                                This.N_Hop_Last_In_Seq;
      This.N_Hop_Last_In_Seq := Msg.Seq;

      if This.Succ.Id /= No_Node then
         This.Link.Send (New_Address (This.Succ.Id, This.Channel), Msg);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Tail_To_Head_Status;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class)
   is
      pragma Unreferenced (From);
      use Sancta.Network;
   begin
      This.Log_Time  := Msg.Log_Time;
      This.Log_Delta.Reset;
      This.Last_Reverse_Status := Msg;
      This.Last_Reverse_Status.Exhausted :=
        This.Last_Reverse_Status.Exhausted and This.Status = Pending_Relay;

      if This.Pred.Id /= No_Node then
         This.Link.Send (New_Address (This.Pred.Id, This.Channel),
                         This.Last_Reverse_Status);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Setup_Request;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class)
   is
      Found : Boolean := False;
   begin
      if This.Role = Base then
         for I in This.Setup_Info.First_Index .. This.Setup_Info.Last_Index loop
            if This.Setup_Info.Element (I).From = From.Sender then
               Found := True;
               declare
                  Info : Robot_Info := This.Setup_Info.Element (I);
               begin
                  Info.Pose := Msg.Pose;
                  This.Setup_Info.Replace_Element (I, Info);
                  exit;
               end;
            end if;
         end loop;

         if not Found then
            This.Setup_Info.Append ((From => From.Sender,
                                     Pose => Msg.Pose,
                                     others => <>));
         end if;

         Log ("Setup info incoming from " & (-From.Sender) & " New: " &
              Boolean'Image (not Found),
              Debug, Log_Section);
      end if;
   end Process;

   -------------
   -- Process --
   -------------

   procedure Process (Msg  : Msg_Setup_Reply;
                      From : Sancta.Network.Message_Metadata;
                      This : in out Object'Class)
   is
      use Id_Vectors;
   begin
      if From.Sender = This.Link.Id then
         return; -- We're base, do nothing
      end if;

      if Msg.Tail_To_Head.Is_Empty then
         null; -- Not enough info yet
      elsif Natural (Msg.Tail_To_Head.Length) = 1 then
         if Msg.Tail_To_Head.First_Element = This.Link.Id then
            This.Role    := Tailhead;
            This.Pred.Id := No_Node;
            This.Succ.Id := This.Base_Id;
            This.Succ.Pose := This.Base_Pose;
         end if;
      elsif Msg.Tail_To_Head.First_Element = This.Link.Id then
         This.Role      := Tail;
         This.Pred.Id   := Element (Next (Msg.Tail_To_Head.First));
         This.Succ.Id   := This.Base_Id;
         This.Succ.Pose := This.Base_Pose;
      elsif Msg.Tail_To_Head.Last_Element = This.Link.Id then
         This.Role    := Head;
         This.Pred.Id := No_Node;
         This.Succ.Id := Element (Previous (Msg.Tail_To_Head.Last));
      else
         This.Role := Relay;
         for I in Msg.Tail_To_Head.First_Index + 1 .. Msg.Tail_To_Head.Last_Index - 1 loop
            if Msg.Tail_To_Head.Element (I) = This.Link.Id then
               This.Pred.Id := Msg.Tail_To_Head.Element (I + 1);
               This.Succ.Id := Msg.Tail_To_Head.Element (I - 1);
            end if;
         end loop;
      end if;

      if This.Setup_Last /= Msg.Tail_To_Head then
         This.Setup_Last := Msg.Tail_To_Head;

         Log ("Configured chain (head to base):", Informative, Log_Section);
         for I in reverse Msg.Tail_To_Head.First_Index .. Msg.Tail_To_Head.Last_Index loop
            Log ("   " & (-Msg.Tail_To_Head.Element (I)), Informative, Log_Section);
         end loop;
         Log ("   " & (-This.Base_Id), Informative, Log_Section);
      end if;
   end Process;

   ---------------------
   -- Update_Neighbor --
   ---------------------

   procedure Update_Neighbor (This : in out Neighbor_Context;
                              Msg  :        Msg_Robot_Local_Update'Class)
   is
      use Sancta.Types.Operations;
   begin
      if Msg.From = This.Id then
         This.Status := Msg.Status;
         This.Dist   := Distance (This.Parent.Pose, Msg.Pose);
         This.Parked := Msg.Parked;
         This.Pose   := Msg.Pose;
      end if;
   end Update_Neighbor;

   --------------
   -- Rel_Dist --
   --------------

   function Rel_Dist (This : Object; Dist : Types.Real) return Relative_Dist is
   begin
      if Dist <= This.Config.Here_Dist_Threshold then
         return Here;
      elsif Dist <= This.Config.Near_Dist_Threshold then
         return Near;
      else
         return Far;
      end if;
   end Rel_Dist;

   -------------------
   -- Location_Back --
   -------------------

   procedure Location_Back (This : in out Object) is
   begin
      if This.Loc.Get /= This.Branch.First_Element then
         This.Loc.Set (Sancta.Map.Prev (This.Branch, This.Loc.Get));
      end if;
   end Location_Back;

   ----------------------
   -- Location_Forward --
   ----------------------

   procedure Location_Forward (This : in out Object) is
   begin
      if This.Loc.Get /= This.Branch.Last_Element then
         This.Loc.Set (Sancta.Map.Next (This.Branch, This.Loc.Get));
      end if;
   end Location_Forward;

   --------------
   -- Loc_Pose --
   --------------

   function Loc_Pose (This : Object) return Types.Pose is
   begin
      if This.Loc.Is_Valid then
         return This.M.Nearest_Pose (This.Loc.Get);
      else
         return This.Pose;
      end if;
   end Loc_Pose;

   --------------------
   -- Team_Exhausted --
   --------------------

   function Team_Exhausted (This : Object) return Boolean is
      Exhausted : Boolean;
   begin
      if This.Role = Tailhead then
         Exhausted :=
           This.Status = Pending_Relay or else
           This.Update_Timer.Elapsed > 2.0 * This.Config.Fixed_Relay_Period;
      else
         Exhausted := This.Last_Reverse_Status.Exhausted;
      end if;

      if not Exhausted then
         This.Self.Exhaust_Timer.Reset;
      end if;

      return This.Mission_Status = Mission_Snafu and then Exhausted and then
             This.Exhaust_Timer.Elapsed > This.Config.Fixed_Relay_Period * 2.0;
   end Team_Exhausted;

   --------------
   -- Data_Log --
   --------------

   procedure Log_Data (This : in out Object) is
      Link : Sancta.Network.Layer.Root.Object'Class renames
        Sancta.Network.Layer.Root.Object'Class (This.Link.all);

      use Agpl.Conversions;
      use Sancta.Ctree.Stats;

      function S is new To_Str (Types.Real);
      function S is new To_Str (Types.Angle);
      function S is new To_Str (Float);
      function S is new Fixed_To_Str (Signal_Q);
   begin
      if False and then This.Mission_Status < Mission_Snafu then
         return;
      end if;

      This.Stats.Update (This.To_Assignment, This.Dummy_Links, This.Jobs);

      if This.First_Log_Data then
         This.First_Log_Data := False;
         This.Logger_Ctree.Log
           ("#DATA# Epoch Elapsed Stat Xi Yi Ai Dij Di0 Q UT " &
            "BWin BWout BWsum " &
            "1-hop-PacketIn 1-hop-PacketLost 1-hop-%Lost " &
            "N-hop-PacketIn N-hop-PacketLost N-hop-%Lost " &
            "SilenceP " &
            "SumOdom MaxOdom AveTime",
            Always, Log_Section);
      end if;

      declare
         use Sancta.Types.Operations;
         Bwin : constant Float := Link.Avg_Bw_In;
         Bwou : constant Float := Link.Avg_Bw_Out;
         Base : Types.Pose;
      begin
         if This.Role = Distributed.Base then
            Base := This.Pose;
         else
            Base := This.Base_Pose;
         end if;
         This.Logger_Ctree.Log
           ("#DATA# " &
            S (Float (Agpl.Chronos.Epoch.Elapsed)) & " " &
            S (Float (This.Log_Time + This.Log_Delta.Elapsed)) & " " &
            This.Status'Img                        & " " &
            S (This.Pose.X)                        & " " &
            S (This.Pose.Y)                        & " " &
            S (This.Pose.A)                        & " " &
            S (This.Succ.Dist)                     & " " &
            S (Distance (This.Pose, Base))         & " " &
            S (This.Succ.Q)                        & " " &
            S (This.Config.Signal_Threshold)       & " " &
            S (Bwin)                     & " " &
            S (Bwou)                     & " " &
            S (Bwin + Bwou)              & " " &

            S (This.One_Hop_Msg_Count)                     & " " &
            S (This.One_Hop_Lost_Msgs)                     & " " &
            S (Float (This.One_Hop_Msg_Count) * 100.0 /
                Float (Natural'Max (1, This.One_Hop_Lost_Msgs)))   & " " &

            S (This.N_Hop_Msg_Count)                     & " " &
            S (This.N_Hop_Lost_Msgs)                     & " " &
            S (Float (This.N_Hop_Msg_Count) * 100.0 /
                Float (Natural'Max (1, This.N_Hop_Lost_Msgs)))   & " " &

            S (Float (This.Silence_Timer.Elapsed)) & " " &
            S (This.Stats.Current_Values (Min_Sum_Odom))  & " " &
            S (This.Stats.Current_Values (Min_Max_Odom))  & " " &
            S (This.Stats.Current_Values (Min_Ave_Time) /
               Float'Max (1.0, Float (This.Jobs_Done.Length))),
            Always, Log_Section);
      end;
   end Log_Data;

   procedure Log_Signal (This : in out Object) is
      Link : Sancta.Network.Layer.Root.Object'Class renames
        Sancta.Network.Layer.Root.Object'Class (This.Link.all);

      use Agpl.Conversions;
      use ASU;
      use Sancta.Ctree.Stats;

      function S is new To_Str (Types.Real);
      function S is new To_Str (Types.Angle);
      function S is new To_Str (Float);
      function S is new Fixed_To_Str (Signal_Q);

      Line : Ustring := +"#SIGNAL#";
   begin
      if This.Mission_Status < Mission_Snafu then
         return;
      end if;

      if This.First_Log_Signal then
         This.First_Log_Signal := False;

         declare
            Names : Ustring := +"Base";
         begin
            for I in This.Setup_Last.First_Index .. This.Setup_Last.Last_Index loop
               Asu.Append (Names, " " & Image (This.Setup_Last.Element (I)));
            end loop;

            This.Logger_Signal.Log
              ("#SIGNAL# Epoch Elapsed Xi Yi " & (+Names),
               Always, Log_Section);
         end;
      end if;

      ASU.Append (Line,
                  " " & S (Float (Agpl.Chronos.Epoch.Elapsed))       & " " &
                  S (Float (This.Log_Time + This.Log_Delta.Elapsed)));
      ASU.Append (Line,
                  " " & S (This.Pose.X) &
                  " " & S (This.Pose.Y));

      for I in This.Setup_Last.First_Index .. This.Setup_Last.Last_Index loop
         if This.Qs.Contains (This.Setup_Last.Element (I)) then
            ASU.Append
              (Line,
               " " & S (This.Qs.Element (This.Setup_Last.Element (I))));
         else
            ASU.Append (Line, " 0.0");
         end if;
      end loop;

      This.Logger_Signal.Log (+Line, Always, Log_Section);
   end Log_Signal;

   -------------------
   -- To_Assignment --
   -------------------

   function To_Assignment (This : Object) return Sancta.Assignment.Object is
      A : Assignment.Object;

      use Id_Info_Maps;
      procedure Internal (I : Cursor) is
         Msg : constant Robot_Info := Element (I);
         Bot :          Agent_Proxy.Object;
      begin
         Bot.Set_Name (Image (Msg.From));
         Bot.Set_Pose (Msg.Pose);
         A.Set_Agent (Bot);
      end Internal;

      Bot : Agent_Proxy.Object;
   begin
      This.Last_Global_Status.Info.Iterate (Internal'Access);

      Bot.Set_Name (Image (This.Link.Id));
      Bot.Set_Pose (This.Pose);

      if not This.Jobs.Is_Empty then
         Bot.Set_Task (This.Jobs.First_Element);
      end if;

      A.Set_Agent (Bot);

      return A;
   end To_Assignment;

   -----------------
   -- Target_Pose --
   -----------------

   function Target_Pose (This : Object) return Types.Pose is
   begin
      if This.Target.Is_Valid then
         return Sancta.Tasks.Positioned.Object (This.Target.Ref.all).Pose;
      else
         raise Constraint_Error
           with "No target location, can't get target pose";
      end if;
   end Target_Pose;

   -------------------------
   -- Is_Commanding_Robot --
   -------------------------

   function Is_Commanding_Robot (This : Object) return Boolean is
   begin
      return This.Commanding_Robot and then not (This.Role = Base);
   end Is_Commanding_Robot;

   -------------
   -- Is_Head --
   -------------

   function Is_Head (This : Object) return Boolean is
   begin
      return This.Role = Head or else This.Role = Tailhead;
   end Is_Head;

   -------------
   -- Is_Tail --
   -------------

   function Is_Tail (This : Object) return Boolean is
   begin
      return This.Role = Tail or else This.Role = Tailhead;
   end Is_Tail;

   ---------
   -- Loc --
   ---------

   function Succ_Loc (This : Object) return Sancta.Map.Location'Class is
   begin
      return This.Map.Ref.Nearest_Location (This.Succ.Pose);
   end Succ_Loc;

end Sancta.Ctree.Distributed;
