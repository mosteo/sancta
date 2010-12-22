with Gtk.Handlers; pragma Elaborate_All (Gtk.Handlers);
with Gtk.Widget; use Gtk.Widget;

--  Used to connect widgets with the data

package Sancta.Gui.Visor_Widget.Handlers_U is new
Gtk.Handlers.User_Callback
  (Gtk_Widget_Record,
   Visor_Widget.Object_Access);
