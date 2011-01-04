with Agpl.Gdk.Custom_Widget;
with Agpl.Gdk.Widget_Bundle;
with Sancta.Component;
with Sancta.Component.Cast;
with Sancta.Ctree.Single_Distributed;

package Sancta.Gtk.Ctree_Node_Status is

--  pragma Preelaborate;

   Component_Name : constant String := "ctree_node_status_to_widget";

   type Object is
     new Sancta.Ctree.Single_Distributed.Node_Status
     and Agpl.Gdk.Custom_Widget.Remote with null record;

   overriding
   function Create (This : Object) return Agpl.Gdk.Widget_Bundle.Object;

   overriding
   procedure Update (This : in out Object;
                     Guts : in out Agpl.Gdk.Widget_Bundle.Object);

   function Cast (From : Sancta.Ctree.Single_Distributed.Node_Status)
                  return Object;

   package Ctree_Node_Status_To_Widget is
     new Sancta.Component.Cast (Component_Name,
                                Ctree.Single_Distributed.Node_Status,
                                Object,
                                Cast);

end SAncta.Gtk.Ctree_Node_Status;
