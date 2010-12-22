with Ada.Containers.Indefinite_Ordered_Maps;
with Agpl.Tasking.Code;
with Agpl.Tasking.Generic_Workers;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Network.Messages;

package body Sancta.Component.Redirect_Listener is

   package SN renames Sancta.Network;

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Network.Network (Helper.Input (Config, Requires_Link)).Link);
   begin
      This.Chan := SN.Value (This.Option (Option_Channel));
      This.Link.Subscribe (SN.Consumer.Object_Access (This), This.Chan);

      This.Wrap := Boolean'Value (This.Option (Option_Wrap, Default_Wrap'Img));
      This.Sink := This.Provided (Provides_Out);

      return Component.Object_Access (This);
   end Create;

   ------------------
   -- On_Reception --
   ------------------

   procedure On_Reception (This : in out Object;
                           M    : in     SN.Message'Class;
                           Meta : in     SN.Message_Metadata)
   is
   begin
      if M in SN.Messages.Redirect_Type'Class then
         declare
            R : SN.Messages.Redirect_Type renames SN.Messages.Redirect_Type (M);
         begin
            if This.Wrap then
               declare
                  Parcel : constant Data_Parcel :=
                    (Owner => Meta.Sender,
                     Label => +String (R.Key.Ref.all),
                     Datum => Data_Handles.Set (R.Val.Ref.all));
               begin
                  if This.Sink then
                     This.Output (Provides_Out, Parcel);
                  else
                     This.Output (External_Key (R.Key.Ref.all), Parcel);
                  end if;
               end;
            else
               if This.Sink then
                  This.Output (Provides_Out, R.Val.Ref.all);
               else
                  This.Output (External_Key (R.Key.Ref.all), R.Val.Ref.all);
               end if;
            end if;

            Log ("Stored redirection: " & R.Key.Ref.all,
                 Debug, Log_Section);

            if R.Ack then
               This.Link.Send
                 (SN.New_Address (Meta.Sender, This.Chan),
                  SN.Messages.Redirect_Ack'
                    (Key => R.Key,
                     Val => R.Val));
            end if;
         end;
--        else
--           Log ("Discarding non-redirect unexpected message: " &
--                External_Tag (M'Tag), Debug, Log_Section);
      end if;
   end On_Reception;

   ---------
   -- Set --
   ---------

   procedure Set (Key  : External_Key;
                  Val  : Data'Class;
                  Link : SN.Layer.Object_Access;
                  Addr : SN.Address)
   is
   begin
      Link.Send (Addr,
                 SN.Messages.Redirect (String (Key), Val));
   end Set;

   ----------
   -- ACKs --
   ----------

   package body ACKs is

      type ACK_Key (Key_Length : Positive) is record
         Id  : Node_Id;
         Key : String (1 .. Key_Length);
      end record;

      function "<" (L, R : ACK_Key) return Boolean is
      begin
         if L.Id < R.Id then
            return True;
         elsif L.Id = R.Id and then L.Key < R.Key then
            return True;
         else
            return False;
         end if;
      end "<";

      type Waited_Data is record
         Data   : Data_Handle;
         Serial : Natural;
      end record;

      use type Data'Class;
      package ACK_Key_Val_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps (ACK_Key, Waited_Data);

      ----------------
      -- ACK_Waiter --
      ----------------

      protected ACK_Waiter is

         procedure Set_Send
           (Key : ACK_Key; Sent : Data'Class; Serial : out Natural);

         procedure Set_ACKd (Key : ACK_Key; ACKd : Data'Class);

         procedure Check (Key    :     ACK_Key;
                          Serial : out Natural;
                          OK     : out Boolean);
         --  If there's a match, data is purged and we're done.
         --  Serial is provided to abort a superceded iterator

         entry Wait_For_ACK;

      private

         Series  : Natural := 1; -- Sequence for Serials
         --  0 is reserved for failed ACKs

         New_ACK : Boolean := False;

         Sent    : ACK_Key_Val_Maps.Map; -- The ones sent and expected
         ACKd    : ACK_Key_Val_Maps.Map; -- The ones got back

      end ACK_Waiter;

      ----------------
      -- ACK_Waiter --
      ----------------

      protected body ACK_Waiter is

         --------------
         -- Set_Send --
         --------------

         procedure Set_Send
           (Key : ACK_Key; Sent : Data'Class; Serial : out Natural) is
         begin
            ACK_Waiter.Sent.Include (Key, (Data_Handles.Set (Sent), Series));
            Serial := Series;
            Series := Series + 1;
         end Set_Send;

         --------------
         -- Set_ACKd --
         --------------

         procedure Set_ACKd (Key : ACK_Key; ACKd : Data'Class) is
         begin
            ACK_Waiter.ACKd.Include (Key, (Data_Handles.Set (ACKd), 0));
            --  Note that there's no serial in the ACK, so we are making this up
            pragma Take_Care;
            New_ACK := True;
         end Set_ACKd;

         -----------
         -- Check --
         -----------

         procedure Check (Key    :     ACK_Key;
                          Serial : out Natural;
                          OK     : out Boolean)
         is
         begin
            OK :=
              Sent.Contains (Key) and then ACKd.Contains (Key) and then
              Sent.Element (Key).Data.Get = ACKd.Element (Key).Data.Get;

            if Sent.Contains (Key) then
               Serial := Sent.Element (Key).Serial;
            else
               Serial := 0;
            end if;

            if OK then
               ACK_Waiter.Sent.Exclude (Key);
               ACK_Waiter.ACKd.Exclude (Key);
            end if;
         end Check;

         ------------------
         -- Wait_For_ACK --
         ------------------

         entry Wait_For_ACK when New_ACK is
         begin
            New_ACK := False;
         end Wait_For_ACK;

      end ACK_Waiter;

      type ACK_Listener_Type is new SN.Consumer.Object with null record;

      overriding
      procedure On_Reception (This : in out ACK_Listener_Type;
                              M    : in     SN.Message'Class;
                              Meta : in     SN.Message_Metadata);

      procedure On_Reception (This : in out ACK_Listener_Type;
                              M    : in     SN.Message'Class;
                              Meta : in     SN.Message_Metadata)
      is
         pragma Unreferenced (This);
      begin
         if M in SN.Messages.Redirect_ACK then
            declare
               R : SN.Messages.Redirect_ACK renames SN.Messages.Redirect_ACK (M);
            begin
               ACK_Waiter.Set_ACKd
                 ((R.Key.Get'Length, Meta.Sender, R.Key.Get),
                  R.Val.Get);
            end;
         end if;
      end On_Reception;

      ACK_Listener : aliased ACK_Listener_Type;

      ------------------
      -- Set_Blocking --
      ------------------

      procedure Set_Blocking (Key   : External_Key;
                              Val   : Data'Class;
                              Link  : SN.Layer.Object_Access;
                              Dest  : Node_Id;
                              Chan  : Sancta.Network.Channel;
                              Retry : Duration;
                              OK    : out Boolean)
      is
         use SN.Messages.Datastore_Key_Handle;
         use SN.Messages.Datastore_Object_Handle;
         AKey : constant ACK_Key := (Key'Length, Dest, String (Key));
         Serial : Natural;
      begin
         Link.Subscribe (ACK_Listener'Access, Chan);
         --  Duplicates taken care at SNL.Root

         loop
            Link.Send
              (SN.New_Address (Dest, Chan),
               SN.Messages.Redirect_Type'
                 (Key => +String (Key),
                  Val => +Val,
                  ACK => True));

            ACK_Waiter.Set_Send (AKey, Val, Serial);

            select
               ACK_Waiter.Wait_For_ACK;
               declare
                  Serial_Check : Natural;
               begin
                  ACK_Waiter.Check (AKey, Serial_Check, OK);
                  if OK then
                     Log ("Set_Blocking: exiting, ACKd", Debug, Log_Section);
                     exit;
                  elsif Serial /= Serial_Check then
                     Log ("Set_Blocking: exiting, superseded (1)",
                          Warning, Log_Section);
                     OK := False;
                     exit;
                  end if;
               end;
            or
               delay Retry;
               declare
                  Serial_Check : Natural;
               begin
                  ACK_Waiter.Check (AKey, Serial_Check, OK);
                  if Serial /= Serial_Check then
                     Log ("Set_Blocking: exiting, superseded (2)",
                          Warning, Log_Section);
                     OK := False;
                     exit;
                  end if;
               end;
            end select;
         end loop;
      end Set_Blocking;

      type Inner (Key_Len : Positive) is new Agpl.Tasking.Code.Object with record
         Context : Context_Type;
         Key     : External_Key (1 .. Key_Len);
         Val     : Data_Handle;
         Link    : SN.Layer.Object_Access;
         Dest    : Node_Id;
         Chan    : SN.Channel;
         Retry   : Duration;
         CB      : Callback;
      end record;

      overriding
      procedure Run (This : in out Inner);

      procedure Run (This : in out Inner) is
         OK : Boolean := False;
      begin
         Set_Blocking (This.Key,
                       This.Val.Get,
                       This.Link,
                       This.Dest,
                       This.Chan,
                       This.Retry,
                       OK);

         if OK and then This.CB /= null then
            This.CB (This.Context,
                     This.Key,
                     This.Val.Get);
         end if;
      end Run;

      function Same (L, R : Inner) return Boolean is
      --  Dunno why isn't there a default "=" for Inner created by the compiler
      --  In the end, the Class "=" below must be dispatching to something!
      begin
         return Inner'Class (L) = Inner'Class (R);
      end Same;

      package Workers is new Agpl.Tasking.Generic_Workers (Inner, Same);

      ---------
      -- Set --
      ---------

      procedure Set (Ctext : Context_Type;
                     Key   : External_Key;
                     Val   : Data'Class;
                     Link  : SN.Layer.Object_Access;
                     Dest  : Node_Id;
                     Chan  : Sancta.Network.Channel;
                     Retry : Duration;
                     CB    : Callback)
      is
      begin
         Workers.Launch
           (Inner'
              (Key_Len => Key'Length,
               Context => Ctext,
               Key     => Key,
               Val     => Data_Handles.Set (Val),
               Link    => Link,
               Dest    => Dest,
               Chan    => Chan,
               Retry   => Retry,
               CB      => CB),
            Activate => True);
      end Set;

   end ACKs;

end Sancta.Component.Redirect_Listener;
