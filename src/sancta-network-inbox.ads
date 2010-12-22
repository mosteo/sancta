--  Ready-to-use thread-safe consumer

with Sancta.Network.Consumer;
with Sancta.Network.Handles;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
use  Ada.Containers;

package Sancta.Network.Inbox is

   pragma Preelaborate;

   type Object is limited new Consumer.Object with private;

   type Object_Access is access all Object'Class;

   procedure On_Reception (This : in out Object;
                           M    : in     Message'Class;
                           Meta : in     Message_Metadata);
   --  Will simply store a copy.

   function Is_Empty (This : in Object) return Boolean;

   function Get_First (This : in Object) return Message'Class;
   function Get_First_Metadata (This : in Object) return Message_Metadata;
   --  These two suffer from a nasty race condition, since a new message can
   --  arrive between peeks. Thus, should be considered DEPRECATED

   procedure Remove_First (This : in out Object);
   --  Also not that dandy.

   --  Better use this to retrieve messages:
   procedure Open (This  : in out Object;
                   Read  :        access procedure
                     (Msg  : Message'Class;
                      Meta : Message_Metadata);
                   Block :        Boolean := True);
   --  if Block and no messages, we block until one arrives
   --  if not Block and no messages, immediate return without Read being called.
   --  After each successful Read call, the message is deleted.
   --  NOTE: no attempt at containing exceptions in here.

private

   package Message_Lists is new Indefinite_Doubly_Linked_Lists (Message'Class);
   package Metadata_Lists is new Indefinite_Doubly_Linked_Lists (Message_Metadata);

   protected type Safe_Object is
      procedure Put (M    : in Message'Class;
                     Meta : in Message_Metadata);

      function Get_First return Message'Class;
      function Get_First_Metadata return Message_Metadata;

      function Is_Empty return Boolean;

      procedure Remove_First;

      entry Remove_First_Blocking
        (M     : out Handles.Message_Handle;
         Meta  : out Handles.Metadata_Handle);

      procedure Remove_First_Non_Blocking
        (M     : out Handles.Message_Handle;
         Meta  : out Handles.Metadata_Handle);

--        entry Open_Blocking (Read  : access procedure
--                               (Msg  : Message'Class;
--                                Meta : Message_Metadata));
--    This hangs GNAT2009. Even if not, there's some problem with anonymous access

   private

      Messages : Message_Lists.List;
      Metas    : Metadata_Lists.List;
   end Safe_Object;

   type Object is limited new Consumer.Object with
      record
         Safe : Safe_Object;
      end record;

end Sancta.Network.Inbox;
