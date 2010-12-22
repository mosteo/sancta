with Sancta.Component.Root;
use Sancta;

package Sancta.Component.Qmap is

   --  A quadtree map

   Name                 : aliased constant Component_Name := "qmap";

   Option_File          : constant Option_Attr := "file";
   --  PNG to load

   Option_Vicinity      : constant Option_Attr := "vicinity";
   --  Sancta.Map.Qtree.Vicinities. Does nothing yet.

   Option_X_Left        : constant Option_Attr := "x_left";
   Option_X_Right       : constant Option_Attr := "x_right";
   Option_Y_Bottom      : constant Option_Attr := "y_bottom";
   Option_Y_Top         : constant Option_Attr := "y_top";

   Option_Cell_Min      : constant Option_Attr := "cell_min";
   Option_Cell_Max      : constant Option_Attr := "cell_max";
   --  Min and Max size for a cell, to force/stop division

   Option_Buggy_Stage   : constant Option_Attr := "buggy_stage";
   Default_Buggy_Stage  : constant Boolean     := False;

   Provides_Map         : constant Internal_Key := "map";

   procedure Register;

private

   type Object is new Root.Object with null record;
   type Object_Access is access all Object;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

end Sancta.Component.Qmap;
