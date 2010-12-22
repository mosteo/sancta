with Agpl.Chronos;
with Sancta.Network;
with Sancta.Network.Layer;
with Sancta.Component.Root;

package Sancta.Component.Redirect is

   Log_Section : constant String := "sancta.component.redirect";

   Name : aliased constant String := "redirect";

   Requires_Input       : constant Internal_Key := "input";
   Requires_Link        : constant Internal_Key := "link";

   Requires_Switch      : constant Internal_Key := "switch"; -- Types.Bool
   --  Optional key to monitor; if Exists and False no redirection will take place
   --    independently of the Trigger mode (below).
   --  If this switch is provided, redirection is off until the switch appears.
   --  See for example Component.Redirect_Listener.ACKs for a convenience class.

   Option_New_Key         : constant Option_Attr  := "new_key";
   --  The key under which this data will be relayed, in case the destinator
   --   expects some specific one. If ungiven, EKey (Input) will be used.

   --  In the case that this goes to a Redirect_Listener, this New_Key will
   --  be used as the External_Key (data) in the receiving node.

   Option_Destination   : constant Option_Attr  := "destination";
   --  Node to receive the redirect
   Option_Channel       : constant Option_Attr  := "channel";
   --  And the channel in which it listens

   Option_Trigger       : constant Option_Attr  := "trigger";
   type Triggers is (On_Update, Periodic);
   Default_Trigger      : constant Triggers     := On_Update;
   --  Send every update (on_update) or periodically.

   Option_Period        : constant Option_Attr  := "period";
   Default_Period       : constant Duration     := 0.0;
   --  If Periodic, then one update per period
   --  If on demand, then at most one update per period, only if updated.
   --  Specify period == 0 for all updates

   type Object (<>) is new Root.Object with private;

   procedure Register;

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class) is
     new Root.Object (Name, Config) with
      record
         Dest     : Node_Id;
         Chan     : Network.Channel;
         Trigger  : Triggers := Default_Trigger;
         Period   : Duration := Default_Period;
         Coalesce : Agpl.Chronos.Object;
         Pending  : Boolean  := False;

         On       : Boolean  := True;
      end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Redirect;
