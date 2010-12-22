with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Network.Messages;
with Sancta.Component.Factory;
with Sancta.Component.Ctypes;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Component.Redirect is

   Null_Id : constant String := ".0x.";

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml;
      pragma Warnings (Off);
      Help : constant Helper.Object := Helper.Create (Config);
      pragma Warnings (On);
      This : constant Object_Access :=
               new Object (Name'Access,
                           Config,
                           Network.Network (Help.Input (Requires_Link)).Link);
   begin
--        This.Verify (Option_New_Key);

      This.Trigger :=
        Triggers'Value (This.Option (Option_Trigger, Default_Trigger'Img));

      This.Period  :=
        Duration'Value (This.Option (Option_Period, Default_Period'Img));

      if This.Provided (Requires_Switch) then
         This.On := False;
         This.Subscribe (Requires_Switch);
      end if;

      This.Verify (Option_Destination);
      This.Dest := Value (This.Option (Option_Destination, Null_Id));

      This.Verify (Option_Channel);
      This.Chan := Sancta.Network.Value (This.Option (Option_Channel, ""));

      if This.Trigger = On_Update then
         This.Subscribe (Requires_Input);
      end if;

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      use Network;
   begin
      if Key = Requires_Input then
         if This.On then
            if This.Coalesce.Elapsed >= This.Period then
               This.Coalesce.Reset;
               This.Link.Send
                 (Sancta.Network.New_Address (This.Dest, This.Chan),
                  Sancta.Network.Messages.Redirect
                    (This.Option (Option_New_Key, String (This.Ekey (Key))), Value));
            else
               This.Pending := True;
            end if;
         end if;
      elsif Key = Requires_Switch then
         This.On := Ctypes.Bool (Value).Value;
      end if;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      use Network;

      procedure Do_Send is
         use Sancta.Network;
      begin
         if This.On then
            Log ("SENDING REDIRECT: " &
                 This.Option (Option_New_Key, String (This.Ekey (Requires_Input))) &
                 " to " & Image (This.Dest) & " @chan " & Image (This.Chan),
                 Debug,
                 Log_Section);
            This.Link.all.Send
              (Sancta.Network.New_Address
                 (This.Dest, This.Chan),
               Sancta.Network.Messages.Redirect
                 (This.Option (Option_New_Key, String (This.Ekey (Requires_Input))),
                  This.Input (Requires_Input)));
            Log ("SENT", Debug, Log_Section);
         end if;
      end Do_Send;
   begin
      if This.Period /= 0.0 then
         Next := Clock + This.Period;
         if This.Exists (Requires_Input) then
            case This.Trigger is
               when Periodic =>
                  Do_Send;
                  Next := Clock + This.Period;
               when On_Update =>
                  if This.Pending and then
                     This.Coalesce.Elapsed >= This.Period
                  then
                     This.Pending := False;
                     This.Coalesce.Reset;
                     Do_Send;
                  end if;
                  Next := Next + 0.01;
            end case;
         end if;
      else
         Next := Clock + 10_000.0;
      end if;
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Redirect;
