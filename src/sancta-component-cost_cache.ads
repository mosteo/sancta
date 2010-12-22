with Sancta.Component.Ctypes,
     Sancta.Component.Root,
     Sancta.Cost_Cache;

package Sancta.Component.Cost_Cache is

   --  Just an encapsulation of a cost cache.

   Log_Section : constant String := "sancta.component.cost_cache";

   Name : aliased constant Component_Name := "cost_cache";

   --  <component name="cost_cache" kind="matrix">
   --     <provides data="costs" as="costs" />
   --  </component>

   Provides_Costs : constant Internal_Key := "costs";

   Option_Kind    : constant Option_Attr  := "kind";

   type Kinds is (Matrix);

   type Object (<>) is new Root.Object with private;

   procedure Register;

   subtype Cost_Cache is Ctypes.Cost_Cache;

private

   type Object is new Root.Object with record
      Ptr : Sancta.Cost_Cache.Object_Access;
   end record;

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Stop (This : in out Object);

end Sancta.Component.Cost_Cache;
