with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Top_Pkg.Callbacks is
   procedure On_Nuevo1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Abrir1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Guardar1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Guardar_Como1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Salir1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class);

   procedure On_Acerca_De1_Activate
     (Object : access Gtk_Menu_Item_Record'Class);

   function On_Top_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end Top_Pkg.Callbacks;
