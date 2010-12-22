--  Sample empty component to save-as when creating new ones.

with Sancta.Distributed.Datastore;
with Sancta.Network.Layer;
with Sancta.Component.Root;

package Sancta.Component.Shared_Database is

   Name : aliased constant String := "shared_database";

   Requires_Link     : constant Internal_Key := "link";
   Provides_Database : constant Internal_Key := "database";

   type Object (<>) is new Root.Object with private;

   not overriding
   function Get (This : not null access Object)
                 return                 Distributed.Datastore.Object_Access;

   procedure Register;

private

   type Object is new Root.Object with record
      Link  : access Network.Layer.Object'Class;
      Store :        Distributed.Datastore.Object_Access;
   end record;

   type Object_Access is access all Object'Class;

   overriding
   procedure Stop (This : in out Object);

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

end Sancta.Component.Shared_Database;
