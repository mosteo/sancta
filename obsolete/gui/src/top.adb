with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Top_Pkg; use Top_Pkg;

procedure Top is
   Top : Top_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Top);
   Show_All (Top);
   Gtk.Main.Main;
end Top;
