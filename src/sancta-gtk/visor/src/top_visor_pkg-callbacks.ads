with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Top_Visor_Pkg.Callbacks is

   procedure On_Salir_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Vista_General_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   procedure On_Acerca_De_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

end Top_Visor_Pkg.Callbacks;
