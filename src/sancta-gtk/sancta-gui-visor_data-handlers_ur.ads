with Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;

--  Used to connect widgets with the data

package Sancta.Gui.Visor_Data.Handlers_Ur is new
Gtk.Handlers.User_Return_Callback
  (Gtk_Widget_Record,
   Boolean,
   Visor_Data.Object_Access);
