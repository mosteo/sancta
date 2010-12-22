with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gtk.Widget;

package Sancta.Gui.Canvas is

   type Object (<>) is private;
   --  Use and instance of these to add a visual feedback to the EXPRES gui.

   function Create
     (Name : in String;
      Area : in Gtk.Widget.Gtk_Widget) return Object;
   --  Creates a Canvas association, given a name and a widget properly
   --  connected. Pass this object to the GUI so it will be displayed in the
   --  console.

   function Get_Name (This : in Object) return String;
   --  Returns the name that will be displayed in the console list.

   function Get_Widget (This : in Object) return Gtk.Widget.Gtk_Widget;
   --  Get the widget.

private

   type Object is record
      Name : Ustring;
      Area : Gtk.Widget.Gtk_Widget;
   end record;

end Sancta.Gui.Canvas;
