--  To display the hierarchical tree of tasks.

with Sancta.Controller_Observer;
with Sancta.Plan;

with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;

with Ada.Finalization;

package Sancta.Gui.Plan is

   pragma Elaborate_Body;

   type Object is private;

   function Get_Widget (This : Object) return Gtk_Widget;
   --  Get the widget so it could be displayed.

   procedure Set_Plan (This : in out Object; New_Plan : Sancta.Plan.Object);
   --  Set the new plan to display

   --  Synch object
   type Synch (Parent : access Object) is new Sancta.Controller_Observer.Object with
     null record;

   procedure Plan_Changed (This : in out Synch; New_Plan : Sancta.Plan.Object);

private

   type Object is new Ada.Finalization.Controlled with
      record
         Data   : Gtk_Tree_Store;
         View   : Gtk_Tree_View;
         Scroll : Gtk_Scrolled_Window;

         Num    : Natural := 0; -- Number of plans received
      end record;

   procedure Initialize (This : in out Object);
   --  Create the tree-view infrastructure.

end Sancta.Gui.Plan;
