with Sancta.Component.Helper;
with Sancta.Component.Network;
with Sancta.Network.Messages;
with Sancta.Component.Factory,
     Sancta.Component.Types;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Component.Redirect_Laser is

   Null_Id : constant String := ".0x.";

   type Object_Access is access all Object;

   procedure Do_Send (This : in out Object) is
   begin
      This.Link.Send
        (Sancta.Network.New_Address
           (This.Dest, This.Chan),
         Sancta.Network.Messages.laser
           (Types.Range_Scan (This.Input (Requires_Laser))));
      Log ("Sending laser redirect", Debug, Log_Section);
   end Do_Send;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml;
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object (Name'Access,
                           Config,
                           Network.Network (Help.Input (Requires_Link)).Link);
   begin
      This.Trigger :=
        Triggers'Value (This.Option (Option_Trigger, On_Update'Img));

      This.Period  :=
        Duration'Value (This.Option (Option_Period, "0.5"));

      This.Verify (Option_Destination);
      This.Dest := Value (This.Option (Option_Destination, Null_Id));

      This.Verify (Option_Channel);
      This.Chan := Sancta.Network.Value (This.Option (Option_Channel, ""));

      if This.Trigger = On_Update then
         This.Subscribe (Requires_Laser);
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
      pragma Unreferenced (Key, Value);
      use Network;
   begin
      if This.Coalesce.Elapsed >= This.Period then
         This.Coalesce.Reset;
         Do_Send (This);
      else
         This.Pending := True;
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
   begin
      if This.Period /= 0.0 then
         Next := Clock + This.Period;
         if This.Exists (Requires_Laser) then
            case This.Trigger is
               when Periodic =>
                  Do_Send (This);
                  Next := Clock + This.Period;
               when On_Update =>
                  if This.Pending and then
                     This.Coalesce.Elapsed >= This.Period
                  then
                     This.Pending := False;
                     This.Coalesce.Reset;
                     Do_Send (This);
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

end Sancta.Component.Redirect_Laser;
