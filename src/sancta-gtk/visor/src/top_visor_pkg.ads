with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Button; use Gtk.Button;
with Gtk.Alignment; use Gtk.Alignment;
with Gtk.Image; use Gtk.Image;
with Gtk.Label; use Gtk.Label;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
package Top_Visor_Pkg is

   type Top_Visor_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Menubar : Gtk_Menu_Bar;
      Ver : Gtk_Menu_Item;
      Stop_Button : Gtk_Button;
      Alignment4 : Gtk_Alignment;
      Hbox4 : Gtk_Hbox;
      Image4 : Gtk_Image;
      Label4 : Gtk_Label;
      Hseparator1 : Gtk_Hseparator;
      Clear_Button : Gtk_Button;
      Alignment3 : Gtk_Alignment;
      Hbox3 : Gtk_Hbox;
      Image3 : Gtk_Image;
      Label3 : Gtk_Label;
      Record_Button : Gtk_Toggle_Button;
      Alignment2 : Gtk_Alignment;
      Hbox2 : Gtk_Hbox;
      Image2 : Gtk_Image;
      Label2 : Gtk_Label;
   end record;
   type Top_Visor_Access is access Top_Visor_Record'Class;

   procedure Gtk_New (Top_Visor : out Top_Visor_Access);
   procedure Initialize (Top_Visor : access Top_Visor_Record'Class);

end Top_Visor_Pkg;
