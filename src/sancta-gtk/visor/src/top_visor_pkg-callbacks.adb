with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

with Expres.Gui.Callbacks;

package body Top_Visor_Pkg.Callbacks is

   use Gtk.Arguments;

   -----------------------
   -- On_Salir_Activate --
   -----------------------

   procedure On_Salir_Activate
     (Object : access Gtk_Image_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Salir_Activate;

   -------------------------------
   -- On_Vista_General_Activate --
   -------------------------------

   procedure On_Vista_General_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Vista_General_Activate;

   ---------------------------
   -- On_Acerca_De_Activate --
   ---------------------------

   procedure On_Acerca_De_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Acerca_De_Activate;

end Top_Visor_Pkg.Callbacks;
