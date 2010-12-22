with Sancta.Network.Layer;

with Ada.Tags; use Ada.Tags;

package Sancta.Network.Consumer.Root is

--   pragma Preelaborate;

   type Object (Link : not null access Layer.Object'Class)
   is abstract new Consumer.Object with null record;
   --  A basic implementation with additional facilities for subscribing.

   type Object_Access is access all Object'Class;

   not overriding
   procedure Subscribe (This     : in out Object;
                        Kind     : in     Tag);
   --  Call with the message kinds you want to be subscribed to.

   not overriding
   procedure Subscribe (This     : in out Object;
                        Kinds    : in     Network.Tag_Array);
   --  Subscription to several messages at once.

   not overriding
   procedure Subscribe (This     : in out Object;
                        Channel  : in     Network.Group_Id);
   --  Subscription to several messages at once.

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Kind     : in Tag);
   --  Call with the message kinds you want to be subscribed to.

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Kinds    : in     Network.Tag_Array);
   --  Subscription to several messages at once.

   procedure Subscribe (This     : in Consumer.Object_Access;
                        Link     : not null access Layer.Object'Class;
                        Channel  : in     Network.Group_Id);
   --  Subscription to several messages at once.

end Sancta.Network.Consumer.Root;
