with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

with Expres.Gui.Main;
with Text_Io; use Text_Io;

package body Top_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------
   -- On_Nuevo1_Activate --
   ------------------------

   procedure On_Nuevo1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Nuevo1_Activate;

   ------------------------
   -- On_Abrir1_Activate --
   ------------------------

   procedure On_Abrir1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Abrir1_Activate;

   --------------------------
   -- On_Guardar1_Activate --
   --------------------------

   procedure On_Guardar1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Guardar1_Activate;

   -------------------------------
   -- On_Guardar_Como1_Activate --
   -------------------------------

   procedure On_Guardar_Como1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Guardar_Como1_Activate;

   ------------------------
   -- On_Salir1_Activate --
   ------------------------

   procedure On_Salir1_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Salir1_Activate;

   ----------------------------
   -- On_Acerca_De1_Activate --
   ----------------------------

   procedure On_Acerca_De1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Acerca_De1_Activate;

   -------------------------
   -- On_Top_Delete_Event --
   -------------------------

   function On_Top_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Expres.Gui.Main.Shutdown := true;

      return False;
   end On_Top_Delete_Event;

end Top_Pkg.Callbacks;
