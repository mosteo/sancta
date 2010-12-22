with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Label; use Gtk.Label;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;
package Top_Pkg is

   type Top_Record is new Gtk_Window_Record with record
      Vbox1 : Gtk_Vbox;
      Menubar : Gtk_Menu_Bar;
      Archivo : Gtk_Menu_Item;
      Menu1 : Gtk_Menu;
      Nuevo1 : Gtk_Image_Menu_Item;
      Abrir1 : Gtk_Image_Menu_Item;
      Guardar1 : Gtk_Image_Menu_Item;
      Guardar_Como1 : Gtk_Image_Menu_Item;
      Separatormenuitem1 : Gtk_Separator_Menu_Item;
      Salir1 : Gtk_Image_Menu_Item;
      Ayuda : Gtk_Menu_Item;
      Menu4 : Gtk_Menu;
      Acerca_De1 : Gtk_Menu_Item;
      Divisor : Gtk_Hpaned;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Consolas : Gtk_Tree_View;
      Etiqueta_Consolas : Gtk_Label;
      Statusbar : Gtk_Statusbar;
   end record;
   type Top_Access is access Top_Record'Class;

   procedure Gtk_New (Top : out Top_Access);
   procedure Initialize (Top : access Top_Record'Class);

end Top_Pkg;
