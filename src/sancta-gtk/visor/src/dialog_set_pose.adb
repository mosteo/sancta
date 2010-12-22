with Gtk; use Gtk;
with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Dialog_Set_Pose_Pkg; use Dialog_Set_Pose_Pkg;

procedure Dialog_Set_Pose is
   Dialog_Set_Pose : Dialog_Set_Pose_Access;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Dialog_Set_Pose);
   Show_All (Dialog_Set_Pose);
   Gtk.Main.Main;
end Dialog_Set_Pose;
