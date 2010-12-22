with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.GEntry; use Gtk.GEntry;
with Glib.Unicode; use Glib.Unicode;
with Gtk.Label; use Gtk.Label;
with Gtk.Button; use Gtk.Button;
with Gtk.Object; use Gtk.Object;
package Dialog_Set_Pose_Pkg is

   type Dialog_Set_Pose_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Table1 : Gtk_Table;
      Entry_X : Gtk_Entry;
      Entry_Y : Gtk_Entry;
      Entry_A : Gtk_Entry;
      Label6 : Gtk_Label;
      Label7 : Gtk_Label;
      Label8 : Gtk_Label;
      Button_Ok : Gtk_Button;
   end record;
   type Dialog_Set_Pose_Access is access Dialog_Set_Pose_Record'Class;

   procedure Gtk_New (Dialog_Set_Pose : out Dialog_Set_Pose_Access);
   procedure Initialize (Dialog_Set_Pose : access Dialog_Set_Pose_Record'Class);

end Dialog_Set_Pose_Pkg;
