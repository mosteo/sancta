with Agpl.Xml,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Distance_Links is

   --  Distance-based links.
   --  Generates a full mesh, not a tree.

   Name                 : aliased constant Component_Name := "distance_links";

   Log_Section          : constant String := "Sancta.Ctree.Component.distance_links";

   Option_Threshold     : constant Option_Attr := "threshold";
   --  Rest length

   Requires_Team        : constant Internal_Key := "team";
   Provides_Links       : constant Internal_Key := "links";

   type Object is new Root.Object with private;
   type Object_Access is access all Object;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   type Object is new Root.Object with null record;

end Sancta.Ctree.Component.Distance_Links;
