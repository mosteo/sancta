with Sancta.Component.Root;
use Sancta;

package Sancta.Component.Bitmap is

   Name                 : aliased constant Component_Name := "bitmap";

   Option_File          : constant Option_Attr := "file";
   Option_Vicinity      : constant Option_Attr := "vicinity";
   --  Sancta.Map.Bitmap.Vicinities
   Option_Cell_Size     : constant Option_Attr := "cell_size";
   Option_Compute_Costs : constant Option_Attr := "compute_costs";

   Provides_Map         : constant Internal_Key := "map";

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   procedure Register;

private

   type Object is new Root.Object with null record;
   type Object_Access is access all Object;

end Sancta.Component.Bitmap;
