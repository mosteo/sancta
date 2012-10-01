--  Rendering of robot data.

with Sancta.Gui.Robot_Data;
with Sancta.Gui.Robot_Data.Messages;
--  with Sancta.Robot;

with Gtk.Box;    use Gtk.Box;
with Gtk.Label;  use Gtk.Label;
with Gtk.Paned;  use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View;  use Gtk.Tree_View;
with Gtk.Widget; use Gtk.Widget;

package Sancta.Gui.Robot is

   pragma Elaborate_Body;

   type Object is new Robot_Data.Messages.Object with private;

   procedure Create (This : out Object; Agent : in out Sancta.Robot.Object);
   --  Initializes the object and adds it to the Agent listeners list.

   function  Get_Widget (This : Object) return Gtk_Widget;
   --  Get the widget which will show the robot data.

   procedure Signal (This : in out Object;
                     Name : in     Robot_Data.Signals;
                     Data : in     Robot_Data.Object_Access);
   --  To receive robot changes notifications.

private

   type Object is new Robot_Data.Messages.Object with
      record
         Left_Box       : Gtk_Box;
         Localized_Pose : Gtk_Label;
         Last_command   : Gtk_Label;
         Pane           : Gtk_Paned;
         Task_Data      : Gtk_Tree_Store;
         Task_Scroll    : Gtk_Scrolled_Window;
         Task_View      : Gtk_Tree_View;
      end record;

end Sancta.Gui.Robot;
