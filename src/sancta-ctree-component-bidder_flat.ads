with Ada.Calendar,
     Agpl.Xml,
     Sancta.Ctree.Ad_Hoc_Bidder,
     Sancta.Agent,
     Sancta.Component,
     Sancta.Component.Root,
     Sancta.Cost_Cache,
     Sancta.Network.Layer;

use Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Bidder_Flat is

   --  Bidder for simple primitive tasks.
   --  Considers explicitly Explore_Directed_Segment tasks

   Log_Section : constant String := "Sancta.Ctree.Component.bidder_flat";

   Name : aliased constant Component_Name := "nerus_bidder_flat";

   Requires_Link  : constant Internal_Key := "link";   -- Network.Layer'Class
   Requires_Agent : constant Internal_Key := "agent";  -- Agent'Class
   Requires_Cost  : constant Internal_Key := "cost";   -- Cost_Cache'Class

   Option_Criterion : constant Option_Attr := "criterion";
   Option_Channel   : constant Option_Attr := "channel";

   procedure Register;

private

   use Sancta;

   type Object (Name   : not null access constant String;
                Config :                 Comp_Config;
                Link   : not null access Network.Layer.Object'Class;
                Cost   : not null access Cost_Cache.Object'Class;
                Bot    : not null access Agent.Object'Class) is
   new Root.Object (Name, Config) with record
      Bdr : Ad_Hoc_Bidder.Object (Link, Cost, Bot);
   end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Ctree.Component.Bidder_Flat;
