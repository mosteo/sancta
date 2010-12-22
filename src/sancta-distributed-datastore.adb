with Sancta.Network.Inbox;

with Agpl.Chronos;
with Agpl.Random;
with Agpl; use Agpl;

with Ada.Calendar; use Ada.Calendar;
with Ada.Task_Identification;

package body Sancta.Distributed.Datastore is

   use type Agpl.Calendar.Serializable_Time.Object;
   use type Node_Id;

   ----------------
   -- Check_Safe --
   ----------------

   procedure Check_Safe (This : Object) is
      use Ada.Task_Identification;
   begin
      if Current_Task = This.Active'Identity then
         raise Program_Error with "Reentrant calls to datastore not allowed";
      end if;
   end Check_Safe;


   --------------
   -- Contains --
   --------------

   function Contains
     (This : not null access Object;
      Key  : in Object_Key)
      return Boolean
   is
      Found : Boolean;
   begin
      This.Check_Safe;
      This.Active.Contains (Key, Found);
      return Found;
   end Contains;

   ------------
   -- Create --
   ------------

   procedure Create (This    : not null access Object;
                     Key     : in              Object_Key;
                     Value   : in out          Object_Data'Class;
                     Success :    out          Boolean)
   is
      Found : Boolean;
   begin
      This.Check_Safe;
      Found := This.Contains (Key);

      if not Found then
         Log ("[Create:Missing:Requested] Key: " & Image (Key),
              Debug, Detail_Section);
         This.Request (Key);

         declare
            Deadline : constant Duration :=
                         1.1 + Duration (Random.Uniform * 1.0);
            Timer    : Chronos.Object;
         begin
            while (not Found) and then Timer.Elapsed < Deadline loop
               delay 0.1;
               Found := This.Contains (Key);
            end loop;
            if not Found then
               This.Set (Key, Value);
               Success := True;
               Log ("[Create:Missing:Added] Key: " & Image (Key),
                    Debug, Detail_Section);
            else
               Success := False;
               Log ("[Create:Missing:Received] Key: " & Image (Key),
                    Debug, Detail_Section);
            end if;
         end;
      else
         Success := False;
         Log ("[Create:Exists] Key: " & Image (Key),
              Debug, Detail_Section);
      end if;
   end Create;

   ----------
   -- Dump --
   ----------

   function Dump (This : in Object) return Object_Maps.Map is
      Result : Object_Maps.Map;
      Data   : Key_Object_Maps.Map;
      procedure Copy (K : in Object_Key; E : in out Object_Entry) is
      begin
         Result.Insert (K, E.Data.Get);
      end Copy;
      procedure Do_It (I : Key_Object_Maps.Cursor) is
      begin
         Data.Update_Element (I, Copy'Access);
      end Do_It;
   begin
      This.Check_Safe;
      This.Active.Dump (Data);
      Data.Iterate (Do_It'Access);
      return Result;
   end Dump;

   ---------
   -- Get --
   ---------

   function Get (This    : not null access Object;
                 Key     : in              Object_Key) return Object_Data'Class
   is
      Value : Object_Data_Handle.Object;
      Meta  : Object_Metadata;
   begin
      This.Get (Key, Value, Meta);
      return Value.Get;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (This    : not null access Object;
                  Key     : in              Object_Key;
                  Value   :    out          Object_Data_Handle.Object;
                  Meta    :    out          Object_Metadata)
   is
      Result : Object_Entry;
   begin
      This.Check_Safe;
      This.Active.Get (Key, Result);
      if Result.Data.Is_Valid then
         Value := Result.Data;
         Meta  := Result.Meta;
      else
         raise Constraint_Error with "Key not present";
      end if;
   end Get;

   --------
   -- Id --
   --------

   function Id (This : in Object) return Node_Id is
   begin
      return This.Link.Id;
   end Id;

   ------------
   -- Listen --
   ------------

   procedure Listen (This     : in out   Object;
                     Key      : in       Object_Key;
                     Listener : not null Key_Listener_Access)
   is
   begin
      This.Check_Safe;
      pragma Assert (Listener /= null);
      This.Active.Listen (Key, Listener);
   end Listen;

   -------------
   -- Request --
   -------------

   procedure Request (This : not null access Object;
                      Key  : in              Object_Key)
   is
   begin
      This.Check_Safe;
      This.Active.Request (Key);
   end Request;

   ---------
   -- Set --
   ---------

   procedure Set (This    : not null access Object;
                  Key     : in              Object_Key;
                  Value   : in out          Object_Data'Class)
   is
   begin
      This.Check_Safe;
      This.Active.Set (Key, Value);
   end Set;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
   begin
      This.Check_Safe;
      select
         This.Active.Shutdown;
      or
         delay 1.0;
         Log ("Aborting unresponsive active task.", Warning, Log_Section);
         abort This.Active;
         Log ("Aborted unresponsive active task.", Warning, Log_Section);
      end select;

      while not This.Active'Terminated loop
         delay 0.1;
      end loop;
   end Shutdown;

   ------------
   -- Update --
   ------------

   procedure Update (This    : not null access Object;
                     Key     : in              Object_Key;
                     Proc    : not null        Update_Callback;
                     Success :    out          Boolean;
                     Tout    : in              Duration := Duration'Last)
   is
   begin
      This.Check_Safe;
      This.Active.Update_Allowed (Key, Proc, Tout, Success);
   end Update;

   -------------------
   -- Active_Object --
   -------------------

   task body Active_Object is

      -----------
      -- Specs --
      -----------

      Callbacks :         Key_Listener_Maps.Map;
      Values    :         Key_Object_Maps.Map;
      Inbox     : aliased Network.Inbox.Object;
      Link      : access  Network.Layer.Object'Class renames Parent.Link;
      This      :         Object renames Object (Parent.all);

      type Update_States is (Allowed, Waiting);

      Update_State       : Update_States := Allowed;
      Update_Tout_Timer  : Chronos.Object;
      Update_Key         : Object_Key := +"";   --  Requested key
      Update_Check_Timer : Chronos.Object;      --  Timer for periodic checks

      procedure Gain_Ownership (Key     : in     Object_Key;
                                Value   : in     Object_Data_Handle.Object;
                                Rev     : in     Revision_Numbers;
                                Src     : in     Action_Sources;
                                Success :    out Boolean;
                                Trigger :    out Triggers);
      --  We will be owners.
      --  When source is local, this is caused by timeouts and Value is not read
      --  When source is remote, this is because awarded ownership and value is
      --   read (and updated if locally newer).

      procedure Lose_Ownership (Value     : in out Object_Entry;
                                New_Owner : in     Node_Id;
                                Success   :    out Boolean);
      --  Remove ownership from a key. If we weren't owners anyway,
      --  Success will be false.

      procedure Notify_Listeners (From  : in     Node_Id;
                                  Value : in     Object_Entry);

      procedure Process_Incoming_Packet
        (M     : in     Message;
         Meta  : in     Network.Message_Metadata;
         Break :    out Boolean);
      --  Break is used to signal that we don't want to process more packets
      --  in this round

      procedure Process_Ownership_Request (Key   : in Object_Key;
                                           From  : in Node_Id;
                                           Since : in Time);

      procedure Process_Trigger (From    : in     Node_Id;
                                 Value   : in     Object_Entry;
                                 Trigger : in     Triggers);

      procedure Send (Msg  : in     Payload);

      procedure Set (Key     : in     Object_Key;
                     Value   : in     Object_Data'Class;
                     Rev     : in     Revision_Numbers;
                     Own     : in     Node_Id;
                     Src     : in     Action_Sources;
                     Success :    out Boolean;
                     Trigger :    out Triggers);

      procedure Update_Metadata_From_Network
        (Key     : in     Object_Key;
         Own     : in     Node_Id;
         Rev     : in     Revision_Numbers;
         Upd     : in     Time;
         Trigger :    out Triggers);

      -----------------
      -- Subprograms --
      -----------------

      ------------------
      -- Add_Listener --
      ------------------

      procedure Add_Listener (Key      : in       Object_Key;
                              Listener : not null Key_Listener_Access)
      is
         use Key_Listener_Maps;
         I  : Cursor := Find (Callbacks, Key);
         Ok : Boolean;
      begin
         if not Has_Element (I) then
            Insert (Callbacks,
                    Key,
                    New_Item => Listener_Vectors.Empty_Vector,
                    Position => I,
                    Inserted => Ok);
            pragma Assert (Ok);
         end if;

         declare
            procedure Add (K : in Object_Key; V : in out Listener_Vectors.Vector)
            is
               pragma Unreferenced (K);
            begin
               V.Append (Listener);
            end Add;
         begin
            Update_Element (Callbacks, I, Add'Access);
         end;
      end Add_Listener;

      ------------------
      -- Check_Object --
      ------------------

      procedure Check_Object (Key : in Object_Key; Value : in out Object_Entry)
      is
      begin
         --  Send ownership if appropriate
         if Value.Meta.Owner = This.Id and then Must_Lose (Value) then
            declare
               Success : Boolean;
            begin
               Lose_Ownership (Value, Value.Best_Owner, Success);
               if Success then
                  Send ((Ownership_Award,
                    Key,
                    Value.Data, Value.Meta.Owner,
                    Value.Meta.Local_Revision,
                    +Value.Meta.Last_Update));
               else
                  Log ("Dist.Datast.Check: Releasing ownership failed for key "
                       & Image (Value), Error, Log_Section);
               end if;
            end;
         end if;

         --  Request ownership if wanted
         if Value.Ownership_Wanted then
            if Value.Meta.Owner = This.Id then
               Value.Ownership_Wanted := False;
            elsif Value.Cron_Own_Request.Elapsed >= Request_Ownership_Period then
               Send ((Ownership_Request, Key, + (Value.Cron_Own_Request.Value)));
               Value.Cron_Own_Request.Reset;
            end if;
         end if;

         --  Force ownership if owner missing:
         if Value.Meta.Owner /= This.Id and then
            Value.Meta.Last_Seen_Owner.Elapsed >=
            3.0 + Duration (Random.Uniform * 2.0) then
            declare
               Success : Boolean;
               Trigger : Triggers;
            begin
               if Value.Data.Is_Valid then
                  Log ("[Event:Own_Dead:Gain] Attempting...",
                       Debug, Detail_Section);
                  Gain_Ownership (Key,
                                  Value.Data,
                                  Value.Meta.Local_Revision,
                                  Local,
                                  Success,
                                  Trigger);
                  pragma Assert (Success,
                                 "Locally forced ownership by owner death");
                  Process_Trigger (This.Id, Value, Trigger);
               end if;
            end;
         end if;

         --  Send metaupdates and reset ownership
         if Value.Meta.Owner = This.Id and then
            Value.Cron_Meta_Update.Elapsed >= 1.0
         then
            Log ("[Event:Meta_Upd:Send:Success] Key: " &
                 Image (Value), Debug, Detail_Section);
            Send ((Metadata_Update, Key,
                   Value.Meta.Local_Revision,
                   +Value.Meta.Last_Update));
            Value.Meta.Last_Seen_Owner.Reset;
            Value.Cron_Meta_Update.Reset;
         end if;
      end Check_Object;

      --------------------
      -- Gain_Ownership --
      --------------------

      procedure Gain_Ownership (Key     : in     Object_Key;
                                Value   : in     Object_Data_Handle.Object;
                                Rev     : in     Revision_Numbers;
                                Src     : in     Action_Sources;
                                Success :    out Boolean;
                                Trigger :    out Triggers)
      is
         use Key_Object_Maps;

         procedure Do_It (K : in Object_Key; X : in out Object_Entry) is
         begin
            pragma Assert (K = Key);
            X.Meta.Local_Revision  :=
              Revision_Numbers'Max (Rev, X.Meta.Local_Revision);
            X.Meta.Newest_Revision :=
              Revision_Numbers'Max (Rev, X.Meta.Local_Revision);
            X.Meta.Owner           := This.Id;
            X.Meta.Last_Update     := Clock;
            X.Meta.Last_Seen_Owner.Reset;
            X.Ownership_Wanted     := False;
            X.Best_Owner           := No_Node; pragma Unsure
              ("He añadido esto y no estoy seguro de las implicaciones, pero 95% pienso que está bien");
         end Do_It;

      begin
         Success := False; -- JIC

         declare
            I : constant Cursor := Values.Find (Key);
         begin
            if Has_Element (I) and then Element (I).Meta.Owner = This.Id then
               Log ("[Gain_Own:Fail:Owned] Key: " & Image (Element (I)),
                    Debug, Detail_Section);
               Success := False;
               return; -- <------- EARLY EXIT (already owners)
            end if;
         end;

         case Src is
            when Local =>
               if not Values.Contains (Key) then
                  raise Constraint_Error; -- Can't be owner of unknown type
               else
                  --  Mark ownership, no change in value
                  declare
                     I : constant Cursor := Values.Find (Key);
                  begin
                     Values.Update_Element (I, Do_It'Access);
                     Success        := True;
                     Trigger.Update := True;
                     Log ("[Gain_Own:Local:Success] Key: " & Image (Element (I)),
                          Debug, Detail_Section);
                  end;
               end if;
            when Remote =>
               declare
                  Local_Success : Boolean;
               begin
                  Set (Key,
                       Value.Ref.all,
                       1,
                       This.Id,
                       Src,
                       Local_Success,
                       Trigger);
                  if Local_Success then
                     Success             := True;
                     Trigger.Request_Own := False;
                     Log ("[Gain_Own:Remote:Udt+Success] Key: " &
                          Image (Values.Element (Key)), Debug, Detail_Section);
                  else
                     --  Local revision was higher,
                     --  we keep ownership and local value:
                     declare
                        I : constant Cursor := Values.Find (Key);
                     begin
                        Values.Update_Element (I, Do_It'Access);
                        Success             := True;
                        Trigger.Update      := True;
                        Trigger.Request_Own := False;
                        Log ("[Gain_Own:Remote:Success] Key: " &
                             Image (Element (I)), Debug, Detail_Section);
                     end;
                  end if;
               end;
         end case;
      end Gain_Ownership;

      ---------
      -- Get --
      ---------

      procedure Get (Key     : in     Object_Key;
                     Value   :    out Object_Entry;
                     Success :    out Boolean;
                     Trigger :    out Triggers)
      is
         use Key_Object_Maps;
         I : constant Cursor := Values.Find (Key);
      begin
         Success := Has_Element (I) and then Element (I).Data.Is_Valid;
         if Has_Element (I) then
            Value := Element (I);
            Log ("[Get:Success] Key: " & Image (Value), Debug, Detail_Section);
         else
            Log ("[Get:Fail] Key: " & Image (Key), Debug, Detail_Section);
         end if;

         Trigger.Request :=
           (not Success) or else
           Value.Meta.Local_Revision < Value.Meta.Newest_Revision;
      end Get;

      --------------------
      -- Lose_Ownership --
      --------------------

      procedure Lose_Ownership (Value     : in out Object_Entry;
                                New_Owner : in     Node_Id;
                                Success   :    out Boolean)
      is
      begin
         Success := Value.Meta.Owner = This.Id;
         if Success then
            Value.Meta.Owner      := New_Owner;
            Value.Best_Owner      := No_Node;
            Value.Best_Owner_Time := Clock;
            Value.Meta.Last_Seen_Owner.Reset;
            Log ("[Lose_Own:Success] Key: " & Image (Value),
                 Debug, Detail_Section);
         else
            Log ("[Lose_Own:Fail:Foreign] Key: " & Image (Value),
                 Debug, Detail_Section);
         end if;
      end Lose_Ownership;

      ----------------------
      -- Notify_Listeners --
      ----------------------

      procedure Notify_Listeners (From  : in     Node_Id;
                                  Value : in     Object_Entry)
      is
      begin
         if Callbacks.Contains (Value.Key) then
            declare
               use Listener_Vectors;
               V : constant Vector := Callbacks.Element (Value.Key);
            begin
               for I in First_Index (V) .. Last_Index (V) loop
                  On_Key_Stored (Element (V, I).all,
                                 From,
                                 Value.Key,
                                 Value.Data.Ref.all,
                                 Value.Meta);
               end loop;
            end;
         end if;
      end Notify_Listeners;

      --------------------
      -- Periodic_Check --
      --------------------

      Checking_Key : Object_Key := +"";

      procedure Periodic_Check is
         use Key_Object_Maps;
         I     : Cursor          := Values.First;
         Watch : Chronos.Object;
      begin
         --  Note above is always true, we do this every cycle!
         --  Can be too much?
         --  We will probably generate too much own_request messages

         if Checking_Key /= +"" then
            I := Values.Find (Checking_Key);
         end if;

         while Has_Element (I) loop
            Values.Update_Element (I, Check_Object'Access);

            Next (I);
            if Watch.Elapsed >= 0.1 then
               Log ("Distr.Datast.Periodic_Check: Overtime",
                    Debug, Log_Section);
               exit;
            end if;
         end loop;

         if Has_Element (I) then
            Checking_Key := Key (I);
         else
            Checking_Key := +"";
         end if;
      end Periodic_Check;

      -------------------
      -- Process_Inbox --
      -------------------

      procedure Process_Inbox is
         Watch : Chronos.Object;
         Break : Boolean := False;
      begin
         while not Inbox.Is_Empty loop
            Process_Incoming_Packet (Message (Inbox.Get_First),
                                     Inbox.Get_First_Metadata,
                                     Break);
            Inbox.Remove_First;

            if Watch.Elapsed >= 0.1 then
               Log ("Distr.Datast.Process_Inbox: Overtime",
                    Debug, Log_Section);
               exit;
            elsif Break then
               Log ("Distr.Datast.Process_Inbox: Breaking out",
                    Debug, Detail_Section);
               exit;
            end if;
         end loop;
      end Process_Inbox;

      -----------------------------
      -- Process_Incoming_Packet --
      -----------------------------

      Seen : Sequence_Numbers := Sequence_Numbers'First;

      procedure Process_Incoming_Packet
        (M     : in     Message;
         Meta  : in     Network.Message_Metadata;
         Break :    out Boolean)
      is
         use Key_Object_Maps;
         Msg  : Payload renames M.Data;
      begin
         Log ("[Msg:Incoming] Seq:" & M.Seq'Img &
              " From " & Image (Meta.Sender) &
              ": " & Msg.Kind'Img, Debug, Log_Section);
--           if M.Seq < Seen then
--              Log ("************** MESSAGE INVERSION:" &
--                   M.Seq'Img & " <" & Seen'Img, Always);
--           end if;
         Seen := Sequence_Numbers'Max (M.Seq, Seen);

         Break := False;

         case Msg.Kind is
            when Value_Request =>
               declare
                  Value   : Object_Entry;
                  Success : Boolean;
                  Trigger : Triggers;
               begin
                  Get (Msg.Key, Value, Success, Trigger);
                  if Success and then Value.Meta.Owner = This.Id then
                     Log ("[On_Reception:Val_Req] Sending update for key " &
                          Image (Value), Debug, Detail_Section);
                     Send ((Value_Update, Msg.Key,
                            Value.Data, Value.Meta.Local_Revision));
                  end if;
                  Trigger.Request := False; -- Prevent chaining of requests!
                  Process_Trigger (Meta.Sender, Value, Trigger);
               end;
            when Value_Update =>
               declare
                  Success : Boolean;
                  Trigger : Triggers;
               begin
                  Set (Msg.Key,
                       Msg.Vu_Value.Ref.all,
                       Msg.Vu_Revision,
                       Meta.Sender,
                       Remote,
                       Success,
                       Trigger);
                  Process_Trigger (Meta.Sender,
                                   Values.Element (Msg.Key),
                                   Trigger);
               end;
            when Ownership_Request =>
               Process_Ownership_Request
                 (Msg.Key, Meta.Sender, +Msg.Or_Waiting_Since);
            when Ownership_Award =>
               if Msg.OA_Owner = This.Id then
                  declare
                     Success : Boolean;
                     Trigger : Triggers;
                  begin
                     Gain_Ownership (Msg.Key,
                                     Msg.Oa_Value,
                                     Msg.Oa_Revision,
                                     Remote,
                                     Success,
                                     Trigger);
                     Process_Trigger (Meta.Sender,
                                      Values.Element (Msg.Key),
                                      Trigger);
                     if Success and then Msg.Key = Update_Key then
                        Break := True;
                     end if;
                  end;
               else
                  --  Mark activity
                  declare
                     Trigger : Triggers;

                  begin
                     Update_Metadata_From_Network
                       (Msg.Key,
                        Meta.Sender,
                        Msg.Oa_Revision,
                        +Msg.Oa_Last_Upd,
                        Trigger);
                     Process_Trigger (Meta.Sender,
                                      Values.Element (Msg.Key),
                                      Trigger);
                  end;
               end if;
            when Metadata_Update =>
               declare
                  Trigger : Triggers;

               begin
                  Update_Metadata_From_Network
                    (Msg.Key,
                     Meta.Sender,
                     Msg.Mu_Revision,
                     +Msg.Mu_Last_Upd,
                     Trigger);
                  Trigger.Notify := False;
                  Trigger.Update := False;
                  Process_Trigger (Meta.Sender,
                                   Values.Element (Msg.Key),
                                   Trigger);
               end;
         end case;
      end Process_Incoming_Packet;

      -------------------------------
      -- Process_Ownership_Request --
      -------------------------------

      procedure Process_Ownership_Request (Key   : in Object_Key;
                                           From  : in Node_Id;
                                           Since : in Time)
      is
         procedure Do_It (K : in Object_Key; Value : in out Object_Entry) is
            pragma Unreferenced (K);
         begin
            if Value.Meta.Owner = This.Id then
               if Value.Best_Owner = No_Node or else
                  (From /= Value.Best_Owner and then
                   Since < Value.Best_Owner_Time)
               then
                  Value.Best_Owner_Time := Since;
                  Value.Best_Owner      := From;
                  Value.Cron_Best_Owner.Reset;
                  Log ("[Req_Own:Accept:Best] Key: " & Image (Value),
                       Debug, Detail_Section);
               elsif From /= Value.Best_Owner then
                  Log ("[Req_Own:Reject:Newer] Key: " & Image (Value),
                       Debug, Detail_Section);
               else
                  null;
               end if;
            else
               Log ("[Own_Req:Failed:Foreign] Key: " & Image (Key),
                 Debug, Detail_Section);
            end if;
         end Do_It;
         use Key_Object_Maps;
         I : constant Cursor := Values.Find (Key);
      begin
         if Has_Element (I) then
            Values.Update_Element (I, Do_It'Access);
         else
            Log ("[Own_Req:Failed:Missing] Key: " & Image (Key),
                 Debug, Detail_Section);
         end if;
      end Process_Ownership_Request;

      ---------------------
      -- Process_Trigger --
      ---------------------

      procedure Process_Trigger (From    : in     Node_Id;
                                 Value   : in     Object_Entry;
                                 Trigger : in     Triggers)
      is
      begin
         null;
         if Trigger.Notify then
            Log ("[Trigger:Notify] Key: " & Image (Value), Debug, Detail_Section);
            Notify_Listeners (From, Value);
         end if;
         if Trigger.Update then
            Log ("[Trigger:Upd] Key: " & Image (Value), Debug, Detail_Section);
            Send ((Value_Update, Value.Key, Value.Data, Value.Meta.Local_Revision));
         end if;
         if Trigger.Request then
            Log ("[Trigger:Req] Key: " & Image (Value), Debug, Detail_Section);
            Send ((Value_Request, Value.Key));
         end if;
         if Trigger.Request_Own then
            Log ("[Trigger:Req_Own] Key: " & Image (Value), Debug, Detail_Section);
            declare
               procedure Do_It (K : Object_Key; X : in out Object_Entry) is
                  pragma Unreferenced (K);
               begin
                  if not X.Ownership_Wanted then
                     X.Cron_Own_Request.Reset;
                     X.Ownership_Wanted := True;
                  end if;
                  Send ((Ownership_Request,
                         Value.Key,
                         +X.Cron_Own_Request.Value));
               end Do_It;
            begin
               Values.Update_Element (Values.Find (Value.Key), Do_It'Access);
            end;
         end if;
      end Process_Trigger;

      -----------------------
      -- Request_Ownership --
      -----------------------

      procedure Request_Ownership (Key : in Object_Key) is
         procedure Do_It (K : Object_Key; X : in out Object_Entry) is
            pragma Unreferenced (K);
         begin
            if not X.Ownership_Wanted then
               X.Ownership_Wanted := True;
               X.Cron_Own_Request.Reset;
            end if;
            Send ((Ownership_Request, Key, +X.Cron_Own_Request.Value));
         end Do_It;
      begin
         Log ("[Req_Own] Key: " & Image (Key), Debug, Detail_Section);
         Values.Update_Element (Values.Find (Key), Do_It'Access);
      end Request_Ownership;

      ----------
      -- Send --
      ----------

      procedure Send (Msg : in Payload)
      is
         Bis : Message := (Network.Message with Msg.Kind, 1, Msg);
      begin
         Msg_Seq.Get_Next (Bis.Seq);
         Log ("[Send] Seq:" & Bis.Seq'Img & " Kind: " & Msg.Kind'Img,
              Debug, Detail_Section);
         Link.Send (Network.New_Address (Database_Channel), Bis);
      end Send;

      ---------
      -- Set --
      ---------

      procedure Set (Key     : in     Object_Key;
                     Value   : in     Object_Data'Class;
                     Rev     : in     Revision_Numbers;
                     Own     : in     Node_Id;
                     Src     : in     Action_Sources;
                     Success :    out Boolean;
                     Trigger :    out Triggers)
      is
         Old          : Object_Entry;
         Get_Success  : Boolean;
         Get_Trigger  : Triggers;
         New_Revision : Revision_Numbers := 1;
      begin
         Get (Key, Old, Get_Success, Get_Trigger);

         if Get_Success then
            New_Revision := Old.Meta.Newest_Revision + 1;
         end if;

         case Src is
            when Local  =>
               Success := True;
               Old.Data.Set (Value);
               if Get_Success then
                  --  UPDATING LOCAL COPY
                  if Old.Meta.Owner /= This.Id then
                     Trigger.Request_Own := True;
                  else
                     Trigger.Update      := True;
                  end if;
                  Old.Meta := (Last_Seen_Owner  => Chronos.Clock,
                               Last_Update      => Clock,
                               Local_Revision   => New_Revision,
                               Newest_Revision  => New_Revision,
                               Owner            => Old.Meta.Owner);
                  if Trigger.Request_Own and then not Old.Ownership_Wanted then
                     Old.Ownership_Wanted := True;
                     Old.Cron_Own_Request.Reset;
                  end if;
               else
                  --  CREATING LOCAL COPY
                  Old.Key := Key;
                  Old.Meta := (Last_Seen_Owner  => Chronos.Clock,
                               Last_Update      => Clock,
                               Local_Revision   => 1,
                               Newest_Revision  => 1,
                               Owner            => This.Id);
                  Trigger.Update := True;
               end if;
               Log ("[Set:Local:Success] Key: " & Image (Old), Debug, Detail_Section);
               Values.Include (Key, Old);
               Trigger.Notify := True;
            when Remote =>
               Success :=
                 (not Get_Success) or else
                 (Old.Meta.Owner /= This.Id and then
                  Old.Meta.Local_Revision < Rev);
               if Success then
                  Old.Key  := Key;
                  Old.Data.Set (Value);
                  Old.Meta := (Last_Seen_Owner  => Chronos.Clock,
                               Last_Update      => Clock,
                               Local_Revision   => Rev,
                               Newest_Revision  => Rev,
                               Owner            => Own);
                  Values.Include (Key, Old);
                  Trigger.Notify  := True;
                  Log ("[Set:Remote:Success] Key: " & Image (Old), Debug, Detail_Section);
               elsif Old.Meta.Owner = This.Id then
                  --  Uh? update for a value we own??
                  --  Release ownership if older or lower
                  if Old.Meta.Local_Revision < Rev or else
                    (Old.Meta.Local_Revision = Rev and then This.Id < Own)
                  then
                     declare
                        Lose_Success : Boolean;
                     begin
                        Lose_Ownership (Old, Own, Lose_Success);
                        pragma Assert (Lose_Success);
                        Old.Meta := (Last_Seen_Owner  => Chronos.Clock,
                                     Last_Update      => Clock,
                                     Local_Revision   => Rev,
                                     Newest_Revision  => Rev,
                                     Owner            => Own);
                        Values.Include (Key, Old);
                        Trigger.Notify  := True;
                        Trigger.Request := True;
                        Log ("[Set:Remote:Lose:Success] Key: " &
                             Image (Old), Debug, Detail_Section);
                     end;
                  else
                     Trigger.Update  := True;
                     --  Someone has to notice we're owners
                     Log ("[Set:Remote:Fail] Key: " & Image (Old),
                          Debug, Detail_Section);
                  end if;
               elsif Old.Meta.Local_Revision > Rev then
                  --  Set for a value we don't own but have higher revision?
                  Trigger.Request_Own := True;
                  Log ("[Set:Remote:Fail] Key: " & Image (Old),
                       Debug, Detail_Section);
               end if;
         end case;
      end Set;

      ------------------------
      -- Update_Can_Proceed --
      ------------------------

      function Update_Can_Proceed return Boolean is
      begin
         return
           Values.Contains (Update_Key) and then
           Values.Element  (Update_Key).Meta.Owner = This.Id;
      end Update_Can_Proceed;

      ---------------------
      -- Update_In_Place --
      ---------------------

      procedure Update_In_Place (I       : in Key_Object_Maps.Cursor;
                                 Updater : in Update_Callback)
      is
         procedure Do_It (Key : in Object_Key; X : in out Object_Entry) is
            Trigger : Triggers;
         begin
            --  Call
            Updater (Key, X.Data.Ref.all, X.Meta);

            X.Meta := (Last_Seen_Owner => Chronos.Clock,
                       Last_Update     => Clock,
                       Local_Revision  => X.Meta.Local_Revision + 1,
                       Newest_Revision => X.Meta.Local_Revision + 1,
                       Owner           => X.Meta.Owner);

            Trigger := (Notify => True,
                        Update => True,
                        others => <>);
            Process_Trigger (This.Id, X, Trigger);
         end Do_It;
      begin
         Values.Update_Element (I, Do_It'Access);
      end Update_In_Place;

      ----------------------------------
      -- Update_Metadata_From_Network --
      ----------------------------------

      procedure Update_Metadata_From_Network
        (Key     : in     Object_Key;
         Own     : in     Node_Id;
         Rev     : in     Revision_Numbers;
         Upd     : in     Time;
         Trigger :    out Triggers)
      is
         use Key_Object_Maps;

         Get_Success  : Boolean;
         Local        : Object_Entry;

         procedure Do_It (Key : Object_Key; X : in out Object_Entry)
         is
            pragma Unreferenced (Key);
         begin
            X.Meta.Last_Seen_Owner.Reset;
            X.Meta.Last_Update     := Upd;
            X.Meta.Newest_Revision :=
              Revision_Numbers'Max (X.Meta.Newest_Revision, Rev);
            X.Meta.Owner         := Own;
         end Do_It;

      begin
         Get (Key, Local, Get_Success, Trigger);
         if Get_Success then
            if Local.Meta.Owner = This.Id then
               --  Uh? update for a value we own??
               --  Release ownership if older or lower
               if Local.Meta.Local_Revision < Rev or else
                 (Local.Meta.Local_Revision = Rev and then This.Id < Own)
               then
                  declare
                     Lose_Success : Boolean;
                  begin
                     Lose_Ownership (Local, Own, Lose_Success);
                     pragma Assert (Lose_Success);
                  end;
                  Trigger.Request := True;
                  Values.Update_Element (Values.Find (Key), Do_It'Access);
                  Log ("[Upd_Meta:Lose:Success] Key: " & Image (Local),
                       Debug, Detail_Section);
               else
                  Trigger.Update  := True; -- Someone has to notice we're owners
                  Log ("[Upd_Meta:Own:Fail] Key: " & Image (Local),
                       Debug, Detail_Section);
               end if;
            else
               Values.Update_Element (Values.Find (Key), Do_It'Access);
               Trigger.Request := Local.Meta.Local_Revision < Rev;
               Log ("[Upd_Meta:Success] Key: " & Image (Local),
                    Debug, Detail_Section);
            end if;
         else
            Values.Include (Key,
              (Key  => Key,
               Data => <>,
               Meta => (Owner           => Own,
                        Newest_Revision => Rev,
                        Last_Update     => Upd,
                        others          => <>),
               others => <>));
            Trigger.Request := True;
            Log ("[Upd_Meta:Missing:Fail] Key: " & Image (Key), Debug, Detail_Section);
         end if;
      end Update_Metadata_From_Network;

      -----------------------
      -- Update_Owner_Seen --
      -----------------------

      procedure Update_Owner_Seen (Key : in Object_Key) is
         procedure Do_It (K : in Object_Key; X : in out Object_Entry) is
            pragma Unreferenced (K);
         begin
            X.Meta.Last_Seen_Owner.Reset;
         end Do_It;
      begin
         Values.Update_Element (Values.Find (Key), Do_It'Access);
      end Update_Owner_Seen;

      ------------------
      -- Update_Reset --
      ------------------

      procedure Update_Reset is
      begin
         Update_State := Allowed;
         Update_Key   := +"";
      end Update_Reset;

      Done : Boolean := False;

   begin
      Link.Subscribe (Inbox'Unchecked_Access, Database_Channel);
      Log ("Distributed datastore task running!", Debug, Detail_Section);

      while not Done loop
         begin

            --  Client calls --

            select
               accept Contains (Key : in Object_Key; Found : out Boolean)
               do
                  Found := Values.Contains (Key) and then
                           Values.Element (Key).Data.Is_Valid;
               end Contains;
            or
               accept Get (Key : in Object_Key; Value : out Object_Entry)
               do
                  declare
                     Success : Boolean;
                     Trigger : Triggers;
                  begin
                     Get (Key, Value, Success, Trigger);
                     Process_Trigger (This.Id, Value, Trigger);
                  end;
               end Get;
            or
               accept Listen (Key      : in Object_Key;
                              Listener :    Key_Listener_Access)
               do
                  Add_Listener (Key, Listener);
               end Listen;
            or
               accept Request (Key  : in Object_Key) do
                  Send ((Value_Request, Key));
               end Request;
            or
               accept Set (Key     : in     Object_Key;
                           Value   : in out Object_Data'Class) do
                  declare
                     Success : Boolean;
                     Trigger : Triggers;
                  begin
                     Set (Key, Value, 1, This.Id, Local, Success, Trigger);
                     pragma Assert (Success, "Local inserts shall succeed");
                     Process_Trigger (This.Id,
                                      Values.Element (Key),
                                      Trigger);
                  end;
               end Set;
            or
               when Update_State = Allowed =>
                  accept Update_Allowed (Key     : in     Object_Key;
                                         Updater : in     Update_Callback;
                                         Timeout : in     Duration;
                                         Success :    out Boolean)
                  do
                     declare
                        use Key_Object_Maps;
                        I : constant Cursor := Values.Find (Key);
                     begin
                        if Has_Element (I) and then
                           Element (I).Meta.Owner = This.Id
                        then
                           Log ("[Upd] Owned, going in. Key: " & Image (Key),
                                Debug, Detail_Section);
                           Success := True;
                           Update_In_Place (I, Updater);
                        else
                           Log ("[Upd] Not owned, requeuing" & Image (Key),
                                Debug, Detail_Section);
                           Update_State   := Waiting;
                           Update_Key     := Key;
                           Update_Tout_Timer.Reset;
                           Update_Check_Timer.Reset;
                           Request_Ownership (Key);
                           requeue Update_Waiting;
                           --  Note not abortable.
                        end if;
                     end;
                  end Update_Allowed;
            or
               when Update_State = Waiting and then
                    (Update_Can_Proceed or else
                     Update_Check_Timer.Elapsed >= 0.1)
                 =>
               accept Update_Waiting (Key     : in     Object_Key;
                                      Updater : in     Update_Callback;
                                      Timeout : in     Duration;
                                      Success :    out Boolean)
               do
                  if Update_Can_Proceed then
                     Log ("[Upd] Obtained, going in", Debug, Detail_Section);
                     Update_In_Place (Values.Find (Key), Updater);
                     Update_Reset;
                     Success := True;
                  elsif Update_Tout_Timer.Elapsed >= Timeout then
                     Log ("[Upd] Timeout, bailing out", Debug, Detail_Section);
                     Update_Reset;
                     Success := False;
                  else
                     Log ("[Upd] Still waiting...", Debug, Detail_Section);
                     --  Still waiting...
                     Update_Check_Timer.Reset;
                     requeue Update_Waiting;
                  end if;
               end Update_Waiting;
            or
               accept Dump (Result : out Key_Object_Maps.Map) do
                  Result := Values;
               end Dump;
            or
               accept Shutdown;
               Done := True;
            or
               delay 0.1;
            end select;

            --  Network messages --
            Process_Inbox;

            --  Periodic checks
            Periodic_Check;

         exception
            when E : others =>
               Log ("Dist.Datast.Active [loop]: " & Report (E),
                    Error, Log_Section);
         end;
      end loop;
      Log ("Dist.Datast.Active exiting normally.", Debug, Log_Section);
   exception
      when E : others =>
         Log ("Dist.Datast.Active [main]: " & Report (E),
              Error, Log_Section);
   end Active_Object;

end Sancta.Distributed.Datastore;
