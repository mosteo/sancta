with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Log_Window_Pkg; use Log_Window_Pkg;

procedure Log_Window is
   Log_Window : Log_Window_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Log_Window);
   Show_All (Log_Window);
   Gtk.Main.Main;
end Log_Window;
