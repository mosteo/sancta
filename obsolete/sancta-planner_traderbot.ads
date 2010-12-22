 

--  A planner which simply auctions received tasks.

with Sancta.Auctioner;
with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Network.Layer;

with Agpl.Xml;
with Agpl; use Agpl;

package Sancta.Planner_Traderbot is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "Plannertrader";
   Detail_Section : constant String := "Plannertrader.detail";
   --  To selectively enable debug messages...

   type Object (Link : not null access Network.Layer.Object'Class)
     is new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   procedure Init (This : in out Object);

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);
   --  Do something with a received packet.

   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Run a control step. Should be called periodically.
   --  If you extend it, you should call the parent Run to keep
   --  current functionality.

   procedure Set_Configuration (This        : in out Object;
                                Config      : in     Xml.Document;
                                Cost_Policy : in Auctioner.Cost_Policies :=
                                  Auctioner.Full_Cost);

private

   type Object (Link : not null access Network.Layer.Object'Class)
     is new Netlistener.Object (Link) with
      record
         Seller : Auctioner.Object (Link);
      end record;

end Sancta.Planner_Traderbot;
