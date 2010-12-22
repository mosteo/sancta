with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Datastore.Types;

with Ada.Unchecked_Deallocation;

--------------------------------------
-- Sancta.Component.Shared_Database --
--------------------------------------

package body Sancta.Component.Shared_Database is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Link  := Network.Network (This.Input (Requires_Link)).Link;
      This.Store := new Distributed.Datastore.Object (This.Link);

      This.Output
        (Provides_Database,
         Datastore.Types.Shared_Database'
           (Datastore.Object_Data with This.Store));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (This : not null access Object)
                 return                 Distributed.Datastore.Object_Access
   is
   begin
      return This.Store;
   end Get;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Distributed.Datastore.Object'Class,
                                    Distributed.Datastore.Object_Access);
   begin
      This.Store.Shutdown;
      Free (This.Store);
   end Stop;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Shared_Database;
