with Agpl.Chronos;
with Sancta.Network;
with Sancta.Network.Layer;
with Sancta.Component.Root;

package Sancta.Component.Redirect_Laser is

   --  A laser reading is too network heavy, so this custom redirector
   --   strips it and sends a laser message directly (instead of a redirect)

   Name : aliased constant String := "redirect_laser";
   Log_Section : constant String := "sancta.component.redirect_laser";

   Requires_Laser       : constant Internal_Key := "laser";
   Requires_Link        : constant Internal_Key := "link";

   Option_Destination   : constant Option_Attr  := "destination";
   --  Node to receive the redirect
   Option_Channel       : constant Option_Attr  := "channel";
   --  And the channel in which it listens

   Option_Trigger       : constant Option_Attr  := "trigger";
   type Triggers is (On_Update, Periodic);
   --  Send every update (on_update) or periodically.

   Option_Period        : constant Option_Attr  := "period";
   --  If Periodic, then one update per period
   --  If on demand, then at most one update per period, only if updated.
   --  Specify period == 0 for all updates

   type Object (<>) is new Root.Object with private;

   --  Any of the following subprograms can be removed entirely if they're not
   --  going to be used, since there's null defaults in the Root class.

   procedure Register;

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class) is
     new Root.Object (Name, Config) with
      record
         Dest     : Node_Id;
         Chan     : Network.Channel;
         Trigger  : Triggers := On_Update;
         Period   : Duration := 0.0;
         Coalesce : Agpl.Chronos.Object;
         Pending  : Boolean  := False;
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

end Sancta.Component.Redirect_Laser;
