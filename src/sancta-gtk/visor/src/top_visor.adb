with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Top_Visor_Pkg; use Top_Visor_Pkg;

procedure Top_Visor is
   Top_Visor : Top_Visor_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Top_Visor);
   Show_All (Top_Visor);
   Gtk.Main.Main;
end Top_Visor;

