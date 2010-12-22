--  Sample empty component to save-as when creating new ones.

with Sancta.Bidder;
limited with Sancta.Located_Agent;
with Sancta.Network.Inbox;
limited with Sancta.Network.Layer;
with Sancta.Component.Root;

package Sancta.Component.Bidder is

   --  Warning. This component uses the Agent in non-thread-safe manner.

   Plugin_Name    : constant String   := "bidder";

   Log_Section    : constant String   := "sancta.Component.bidder";

   Requires_Agent : constant Internal_Key := "agent";
   Requires_Link  : constant Internal_Key := "link";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      Bidder           : aliased Sancta.Bidder.Object;
      Pending_Messages : aliased Network.Inbox.Object;
      Bot              : access Located_Agent.Object'Class;
      Link             : access Network.Layer.Object'Class;
   end record;

end Sancta.Component.Bidder;
