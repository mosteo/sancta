--  Sample empty component to save-as when creating new ones.

with Ada.Finalization,
     Sancta.Component.Player_Client,
     Sancta.Component.Root;

package Sancta.Component.Player_Iface is

   --  Root component with specific utilities to adapt interfaces.
   --  To be used as the base class for components providing a player interface.

   --  On creation, the subscription will happen automatically

   Option_Index  : constant Option_Attr := "index";
   --  Index of this interface, defaults to 0.

   Requires_Client : constant Internal_Key := "client";

   type Object is abstract new Root.Object with private;
   type Object_Access is access all Object'Class;
   --  When creating, we must already provide the Iface object.

   function Index (This : Object) return Natural;

   function Client (This : Object) return Player_Client.Safe_Access;

   function Create_Interface (This : Object)
                              return Player_Client.Iface_Access is abstract;
   --  Must be overriden to provide the new iface object being proxied

   procedure Execute (This : Object;
                      Code : access procedure
                        (Iface : Player_Client.Iface_Access));
   --  Safely execute the player-related Code within the protected context

private

   type Object_Preparer (Parent : access Object) is limited new
     Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Initialize (This : in out Object_Preparer);

   overriding
   procedure Stop (This : in out Object);
   --  Unsubscribe & destroy.

   overriding
   function Requires_Mutex (This : Object) return Boolean;
   --  Nopes, doesn't require

   type Object is abstract new Root.Object with record
      Preparer      : Object_Preparer (Object'Access);

      Cached_Client : Player_Client.Safe_Access;
      Iface         : Player_Client.Iface_Access;
   end record;

   not overriding
   procedure Create_And_Subscribe (This  : in out Object);

end Sancta.Component.Player_Iface;
