with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Glib.Unicode; use Glib.Unicode;
with Gtk.Button; use Gtk.Button;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Image; use Gtk.Image;
with Gtk.Object; use Gtk.Object;
package Dialog_Set_Criterion_Pkg is

   type Dialog_Set_Criterion_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Table1 : Gtk_Table;
      Label1 : Gtk_Label;
      Label2 : Gtk_Label;
      Minmax : Gtk_Entry;
      Minsum : Gtk_Entry;
      Button_Ok : Gtk_Button;
      Alignment1 : Gtk_Alignment;
      Hbox1 : Gtk_Hbox;
      Image1 : Gtk_Image;
      Label3 : Gtk_Label;
   end record;
   type Dialog_Set_Criterion_Access is access Dialog_Set_Criterion_Record'Class;

   procedure Gtk_New (Dialog_Set_Criterion : out Dialog_Set_Criterion_Access);
   procedure Initialize (Dialog_Set_Criterion : access Dialog_Set_Criterion_Record'Class);

end Dialog_Set_Criterion_Pkg;
