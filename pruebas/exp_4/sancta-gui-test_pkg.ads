with Gdk.Event;
with Gtk.Widget; use Gtk.Widget;

package Sancta.Gui.Test_Pkg is

   function Expose_Handler (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose)
                            return Boolean;

   function Expose_Handler2 (Widget : access Gtk_Widget_Record'Class;
                             Event  :        Gdk.Event.Gdk_Event_Expose)
                             return Boolean;

end Sancta.Gui.Test_Pkg;
