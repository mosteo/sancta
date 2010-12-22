with Sancta.Auctioneer.Safe,
     Sancta.Auctions,
     Sancta.Component.Root,
     Sancta.Network.Layer;

package Sancta.Component.Auctioneer is

   --  This type is thread safe (Sancta.Auctioneer is not).

   Log_Section : constant String := "sancta.component.Auctioneer";

   Name : aliased constant Component_Name := "Auctioneer";

   Requires_Link : constant Internal_Key := "link";
   Requires_Item : constant Internal_Key := "item";
   --  Everything written here is put up for auction.
   --  Inputs must be of type Auction.Items'Class

   Option_Channel : constant Option_Attr := "channel";
   --  Where to listen for messages

   --  Configuration example:
   --  <component name="Auctioneer" channel="auctions">
   --     <requires data="link" as="link" />
   --     <requires data="item" as="item" />
   --  </component>

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class) is
     new Root.Object with private;

   procedure Register;

   type Item is new Data with record
      Item : Auctions.Item_Handle.Object;
   end record;

private

   type Object (Name   : access constant String;
                Config :                 Comp_Config;
                Link   : access Network.Layer.Object'Class) is new
     Root.Object (Name   => Name,
                  Config => Config)
   with record
      Auc : Sancta.Auctioneer.Safe.Object;
   end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Auctioneer;
