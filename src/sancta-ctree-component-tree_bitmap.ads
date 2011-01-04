with Ada.Calendar,
     Agpl.Xml,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Tree_Bitmap is

   Name                 : aliased constant Component_Name := "tree_bitmap";

   Log_Section          : constant String := "Sancta.Ctree.Component.tree_bitmap";

   Option_Creator       : constant Option_Attr  := "creator";
   --  type Sancta.Ctree.Tree_Navigator.Bitmap.Creators.

   Requires_Root_Pose   : constant Internal_Key := "root_pose";
   Requires_Map         : constant Internal_Key := "map";
   Requires_Tasks       : constant Internal_Key := "tasks";
   Requires_Must_Replan : constant Internal_Key := "must_replan";

   Provides_Tree        : constant Internal_Key := "tree";

   type Object is new Root.Object with private;
   type Object_Access is access all Object;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return Sancta.Component.Object_Access;

   not overriding
   procedure Replan (This : in out Object);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   type Object is new Root.Object with record
      Done        : Boolean := False;
   end record;

end Sancta.Ctree.Component.Tree_Bitmap;
