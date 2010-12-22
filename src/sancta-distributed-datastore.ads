--  This should be a datastore that's replicated to every other node within
--  reach.

--  *All* messages are broadcasted to help on the maintenaince of other's
--  nodes metadata.

--  Writes are broadcasted, reads are made locally
--  Each value revision is broadcasted periodically, so if a node detects
--  he has missed some update can request the latest revision.

--  To make an update to a value, one has to hold the ownership for it.

--  If a request for ownership goes unanswered for some time and no updates
--  or awards to other nodes are seen in that same time, a final claim
--  is made and at the same time the node forces ownership.

with Sancta.Network.Layer;

package Sancta.Distributed.Datastore is

   --   pragma Elaborate_Body;

   Database_Channel : constant Network.Channel :=
                        Network.Value ("shared_database");

   Log_Section    : constant String := "sancta.distributed.datastore";
   Detail_Section : constant String := "sancta.distributed.datastore.detail";

   type Update_Callback is access
     procedure (Key   : in     Object_Key;
                Value : in out Object_Data'Class;
                Meta  : in     Object_Metadata);

   type Object (Link : access Network.Layer.Object'Class) is
   tagged limited private;
   --  This object is thread safe.

   type Object_Access is access all Object'Class;

   function Contains (This : not null access Object;
                      Key  : in              Object_Key) return Boolean;

   procedure Create (This    : not null access Object;
                     Key     : in              Object_Key;
                     Value   : in out          Object_Data'Class;
                     Success :    out          Boolean);
   --  if a value is not in the DB, store an initial value.
   --  This is race-condition unsafe... some way around it should exist...
   --  It's slow! will delay for a time lasting between 1 .. 3 seconds!
   --  Success will signal if the value has been set or was already there...

   function Get (This    : not null access Object;
                 Key     : in              Object_Key) return Object_Data'Class;
   --  Will never block. Use the other Get if you also want synched metadata

   procedure Get (This    : not null access Object;
                  Key     : in              Object_Key;
                  Value   :    out          Object_Data_Handle.Object;
                  Meta    :    out          Object_Metadata);

   procedure Request (This : not null access Object;
                      Key  : in              Object_Key);
   --  Request refreshing of the value (alas should happen automatically).

   procedure Set (This    : not null access Object;
                  Key     : in              Object_Key;
                  Value   : in out          Object_Data'Class);
   --  Will never block, but if you aren't the owner, a delayed update will
   --  be created for the network, that can be obsoleted by remote writes.
   --  So, in general, this isn't very safe unless you have good reasons to
   --  use it.
   --  Also, it never fails because the revision will be always be the latest
   --  known.

   --  The use of Get/Set is inherently prone to race conditions, not only from
   --  the local POV, but also because our Set can conflict with owner's remote
   --  Sets/Updates. Where this is not desirable, use Update.

   procedure Update (This    : not null access Object;
                     Key     : in              Object_Key;
                     Proc    : not null        Update_Callback;
                     Success :    out          Boolean;
                     Tout    : in              Duration := Duration'Last);
   --  Update in place. This can block for an indetermined period of time,
   --  until ownership is granted. At that point Proc will be called.
   --  DON'T CALL TO THE DATASTORE FROM WITHIN THIS PROCEDURE! It will bang.
   --  Processing within Proc should be short (it's inside a protected!)
   --  Success will be true if the Proc was called before the timeout,
   --  false otherwise

   procedure Listen (This     : in out   Object;
                     Key      : in       Object_Key;
                     Listener : not null Key_Listener_Access);

   procedure Shutdown (This : in out Object);

   function Dump (This : in Object) return Object_Maps.Map;
   --  Get a copy of everything within the datastore

private

   task type Active_Object (Parent : access Object) is

      entry Contains (Key : in Object_Key; Found : out Boolean);

      entry Get (Key : in Object_Key; Value : out Object_Entry);

      entry Listen (Key      : in Object_Key;
                    Listener : in Key_Listener_Access);

      entry Request (Key  : in Object_Key);

      entry Set (Key     : in     Object_Key;
                 Value   : in out Object_Data'Class);

      entry Update_Allowed (Key     : in     Object_Key;
                            Updater : in     Update_Callback;
                            Timeout : in     Duration;
                            Success :    out Boolean);
      --  External guarded call to get parameters

      entry Update_Waiting (Key     : in     Object_Key;
                            Updater : in     Update_Callback;
                            Timeout : in     Duration;
                            Success :    out Boolean);
      --  Internal guarded call, allowing access after owning the key

      entry Dump (Result : out Key_Object_Maps.Map);

      entry Shutdown;

   end Active_Object;

   type Object (Link : access Network.Layer.Object'Class) is
   tagged limited record
      Active : Active_Object (Object'Access);
   end record;

   function Id (This : in Object) return Node_Id;
   pragma Inline (Id);

   procedure Check_Safe (This : Object);
   pragma Inline (Check_Safe);

end Sancta.Distributed.Datastore;
