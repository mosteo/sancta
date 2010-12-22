package Sancta.Network.Consumer is

   pragma Preelaborate;

   type Object is limited interface; -- BUG in GPL 2006 prevents using this

   --  type Object is abstract tagged limited null record;
   --  Use to callback when a message is received.

   type Object_Access is access all Object'Class;

   procedure On_Reception (This : in out Object;
                           M    : in     Message'Class;
                           Meta : in     Message_Metadata) is abstract;
   --  Override to do required processing.

end Sancta.Network.Consumer;
