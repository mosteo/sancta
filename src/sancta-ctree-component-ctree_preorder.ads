with Agpl.Xml,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Ctree_Preorder is

   Name                 : aliased constant Component_Name := "ctree_preorder";

   Log_Section          : constant String := "Sancta.Ctree.Component.ctree_preorder";

   Requires_Tasks       : aliased constant Internal_Key := "tasks";
   Requires_Tree        : aliased constant Internal_Key := "tree";

   Provides_Tasks       : constant Internal_Key := "preorder_tasks";

   type Object is new Root.Object with null record;

   procedure Register;

private

   type Object_Access is access all Object;

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   not overriding
   function Inputs (This : Object) return Internal_Key_Array;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   not overriding
   procedure Process (This : in out Object);

end Sancta.Ctree.Component.Ctree_Preorder;
