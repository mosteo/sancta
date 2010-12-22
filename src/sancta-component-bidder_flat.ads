with Sancta.Agent,
     Sancta.Bidder.Flat,
     Sancta.Component.Environment,
     Sancta.Component.Root,
     Sancta.Cost_Cache,
     Sancta.Network.Layer;

package Sancta.Component.Bidder_Flat is

   --  Bidder for simple primitive tasks.

   Log_Section : constant String := "sancta.component.bidder_flat";

   Name : aliased constant Component_Name := "bidder_flat";

   Requires_Link  : aliased constant Internal_Key := "link";
   --  Network.Layer'Class

   Requires_Agent : aliased constant Internal_Key := "agent";
   --  Agent'Class

   Requires_Cost  : aliased constant Internal_Key := "cost";
   --  Cost_Cache'Class

   Option_Criterion : constant Option_Attr := "criterion";
   Option_Channel   : constant Option_Attr := "channel";

   procedure Register;
   --  Created on demand

private

   type Object (Name   : not null access constant String;
                Config :                 Comp_Config;
                Link   : not null access Network.Layer.Object'Class;
                Cost   : not null access Cost_Cache.Object'Class;
                Bot    : not null access Agent.Object'Class) is
   new Root.Object (Name, Config) with record
      Bdr : Bidder.Flat.Object (Link, Cost, Bot);
   end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    : Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Bidder_Flat;
