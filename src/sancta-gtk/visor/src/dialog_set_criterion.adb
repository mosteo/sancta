with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Dialog_Set_Criterion_Pkg; use Dialog_Set_Criterion_Pkg;

procedure Dialog_Set_Criterion is
   Dialog_Set_Criterion : Dialog_Set_Criterion_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Dialog_Set_Criterion);
   Show_All (Dialog_Set_Criterion);
   Gtk.Main.Main;
end Dialog_Set_Criterion;
