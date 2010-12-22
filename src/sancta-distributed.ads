--  This should be a datastore that's replicated to every other node within
--  reach.

--  Writes are broadcasted, reads are made locally
--  Each value revision is broadcasted periodically, so if a node detects
--  he has missed some update can request the latest revision.

--  To make an update to a value, one has to hold the token for it.
--  There is a target outbound bandwidth not to be exceeded.
--  All of this is transparently managed.

with Sancta.Network;

with Agpl.Calendar.Serializable_Time;
with Agpl.Chronos;
with Agpl.Generic_Handle;
with Agpl.Sequence;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package Sancta.Distributed is

--   pragma Elaborate_Body;

   Timeout : exception;

   Data_Not_Present : exception;

   type Revision_Numbers is new Positive;
   --  Used to track the most up-to-date value.

   type Synch_States is (Up_To_Date, Old, Unknown);

   --  This metadata record holds info that clients are allowed to see
   type Object_Metadata is record
      Last_Seen_Owner : Agpl.Chronos.Object;
      --  Last time we saw the owner of this

      Last_Update     : Ada.Calendar.Time;
      --  Timestamp of the last update made (local time of the updater).

      Local_Revision  : Revision_Numbers := 1;
      --  Revision we have stored.

      Newest_Revision : Revision_Numbers := 1;
      --  Newest revision that we have knowledge of existence.

      Owner           : Node_Id := No_Node;
      --  Node who owns the right to update this value.
   end record;

   type Object_Data is tagged null record;
   --  Extend this type to suit your needs

   function Image (This : in Object_Data) return String;
   --  For debugging purposes, default returns the External_Tag

   package Object_Data_Handle is new Agpl.Generic_Handle (Object_Data'Class);

   type Object_Key is new Ustring;

   function Value (S : in String) return Object_Key;
   function "+"   (S : in String) return Object_Key renames Value;
   pragma Inline (Value);

   function Image (K : in Object_Key) return String;
   pragma Inline (Image);

   function Image (K : in Object_Key; M : in Object_Metadata) return String;
   pragma Inline (Image);

   type Key_Listener is abstract tagged limited null record;

   type Key_Listener_Access is access all Key_Listener'Class;

   procedure On_Key_Stored (This  : in out Key_Listener;
                            From  : in     Node_Id;
                            Key   : in     Object_Key;
                            Value : in     Object_Data'Class;
                            Meta  : in     Object_Metadata)
   is abstract;

   type Message (<>) is new Network.Message with private;

   package Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Object_Key, Object_Data'Class);

private

   Request_Ownership_Period : constant Duration := 0.1;
   --  Period for sending request messages when wanting ownership.

   Hold_Ownership_Period    : constant Duration := 0.1;
   --  Once we receive a ownership request, we hold it for this time
   --  expecting an older request. Each older request resets this counter.


   use type Node_Id;

   type Object_Entry is tagged record
      Key              : Object_Key; -- For convenience

      Cron_Meta_Update : Agpl.Chronos.Object; -- For updates on metadata

      Ownership_Wanted : Boolean := False; --  Are we waiting to be owners?
      Cron_Own_Request : Agpl.Chronos.Object; -- For our ownership requests

      Best_Owner_Time  : Ada.Calendar.Time;
      --  Time of request of the best candidate for ownership (oldest one).
      Best_Owner       : Node_Id := No_Node;
      Cron_Best_Owner  : Agpl.Chronos.Object;
      --  We delay for some time awarding ownership.
      --  In this way we avoid starvation by collusion.

      Meta : Object_Metadata; -- Exposed to clients
      Data : Object_Data_Handle.Object;
   end record;

   function Image (Value : in Object_Entry) return String;

   function Must_Lose (This : in Object_Entry) return Boolean;
   pragma Inline (Must_Lose);

   type Message_Kinds is (Value_Request,
                          Value_Update,
                          Ownership_Request,
                          Ownership_Award,
                          Metadata_Update);

   type Action_Sources is (Local, Remote);

   --  These are possible triggered happenings when setting values in our
   --  database;
   type Triggers is record
      Notify      : Boolean := False; -- Call listeners
      Update      : Boolean := False; -- Send network update
      Request     : Boolean := False; -- Request network refresh
      Request_Own : Boolean := False; --  We must request ownership
   end record;

   type Sequence_Numbers is mod Positive'Last;
   package Msg_Seqs is new Agpl.Sequence (Sequence_Numbers);

   Msg_Seq : Msg_Seqs.Object;

   type Payload (Kind : Message_Kinds) is record
      Key  : Object_Key;
      case Kind is
         when Value_Update =>
            Vu_Value    : Object_Data_Handle.Object;
            Vu_Revision : Revision_Numbers;
         when Ownership_Award =>
            Oa_Value    : Object_Data_Handle.Object;
            Oa_Owner    : Node_Id;
            Oa_Revision : Revision_Numbers;
            Oa_Last_Upd : Agpl.Calendar.Serializable_Time.Object;
         when Metadata_Update =>
            Mu_Revision : Revision_Numbers;
            Mu_Last_Upd : Agpl.Calendar.Serializable_Time.Object;
         when Ownership_Request =>
            Or_Waiting_Since : Agpl.Calendar.Serializable_Time.Object;
         when Value_Request =>
            null;
      end case;
   end record;

   type Message (Kind : Message_Kinds) is new Network.Message with record
      Seq  : Sequence_Numbers;
      Data : Payload (Kind);
   end record;

   package Listener_Vectors is new Ada.Containers.Vectors
     (Positive,
      Key_Listener_Access);

   package Key_Listener_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Object_Key,
      Listener_Vectors.Vector,
      "<",
      Listener_Vectors."=");

   package Key_Object_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Object_Key,
      Object_Entry);

end Sancta.Distributed;
