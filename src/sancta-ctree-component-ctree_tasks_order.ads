with Ada.Calendar,
     Agpl.Xml,
     Sancta.Component,
     Sancta.Component.Root;
use Sancta,
    Sancta.Component,
    Sancta.Component.Root;

package Sancta.Ctree.Component.Ctree_Tasks_Order is

   Log_Section       :         constant String := "Sancta.Ctree.Component.ctree_tasks_order";

   Name              : aliased constant Component_Name := "ctree_tasks_order";

   Option_Creator    :         constant Option_Attr  := "creator";

   Requires_Must_Replan
                     : aliased constant Internal_Key := "must_replan";
   Requires_Tasks    : aliased constant Internal_Key := "tasks";
   Requires_Nav_Tree : aliased constant Internal_Key := "nav_tree";

   Provides_Tasks    : constant Internal_Key := "ordered_tasks";

   procedure Register;

private

   type Object is new Root.Object with record
      Done : Boolean := False;
   end record;

   type Object_Access is access all Object;

   function Create (Config : in Agpl.Xml.Node)
                    return      Sancta.Component.Object_Access;

   overriding
   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time);

   not overriding
   function Inputs (This : Object) return Internal_Key_Array;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   not overriding
   procedure Process (This : in out Object);

end Sancta.Ctree.Component.Ctree_Tasks_Order;
