with Sancta.Component.Root;
with Sancta.Network;
with Sancta.Network.Layer;

private with Sancta.Network.Consumer;

package Sancta.Component.Redirect_Listener is

   --  Natural companion to Redirect, it locally stores a value received via
   --  network.

   --  There are also utility functions for programmatically using it from
   --  a remote node.

   Name : aliased constant String := "redirect_listener";

   Log_Section : constant String := "sancta.component.redirect_listener";

   Requires_Link        : constant Internal_Key := "link";
   Provides_Out         : constant Internal_Key := "out";
   --  Optional: if not given, the key is used as external key.
   --  If given, the redirect.key is discarded and this is used instead.
   --  Note that when wrapping is enabled, the redirect.key serves as label.

   Option_Channel       : constant Option_Attr  := "channel";

   Option_Wrap          : constant Option_Attr  := "wrap";
   Default_Wrap         : constant Boolean      := False;
   --  When wrapping takes place, the original data is stored as a Data_Parcel.
   --  Otherwise, the data is stored under its key as-is.

   procedure Register;

   procedure Set (Key  : External_Key;
                  Val  : Data'Class;
                  Link : Network.Layer.Object_Access;
                  Addr : Network.Address);
   --  Simple set. Won't wait for ACKs.
   --  Guarantees on delivery are the same that Link provide...

   generic
      type Context_Type is private;
   package ACKs is
   --  Only one value per recipient/key is waited on.
   --  Once a new call for the same recipient/pair is made, pending ones fail

      procedure Set_Blocking (Key   : External_Key;
                              Val   : Data'Class;
                              Link  : Network.Layer.Object_Access;
                              Dest  : Node_Id;
                              Chan  : Sancta.Network.Channel;
                              Retry : Duration;
                              OK    : out Boolean);
      --  Will block until confirmation is received
      --  OK will be false only if we're superseded by a new Set to the same key
      --    or timeout.

      type Callback is access procedure
                       (Context : Context_Type;
                        Key     : External_Key;
                        Val     : Data'Class);

      procedure Set (Ctext : Context_Type;
                     Key   : External_Key;
                     Val   : Data'Class;
                     Link  : Network.Layer.Object_Access;
                     Dest  : Node_Id;
                     Chan  : Sancta.Network.Channel;
                     Retry : Duration;
                     CB    : Callback);
      --  Will spawn a task to send every Retry seconds the given value.
      --  Once confirmation is received, CB will be called and the task will die.
      --  No multicast due to ACKs.
      --  If this call is superceded by a new one, CB won't be called.
      --  CB can be null, too.
   end ACKs;

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class)
     is new Root.Object (Name => Name, Config => Config)
        and Network.Consumer.Object with
      record
         Chan : Network.Channel;
         Wrap : Boolean;
         Sink : Boolean;
      end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure On_Reception (This : in out Object;
                           M    : in     Network.Message'Class;
                           Meta : in     Network.Message_Metadata);

end Sancta.Component.Redirect_Listener;
