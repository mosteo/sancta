with Gtk.Drawing_Area;
with Gtk.Main;

procedure Sancta.Gui.Testda is
   Draw  : Gtk.Drawing_Area.Gtk_Drawing_Area;
begin
   Gtk.Main.Init;
   Gtk.Drawing_Area.Gtk_New (Draw);
end Sancta.Gui.Testda;
