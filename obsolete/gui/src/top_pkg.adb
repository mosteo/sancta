with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Expres; use Callbacks_Expres;
with Expres_Intl; use Expres_Intl;
with Top_Pkg.Callbacks; use Top_Pkg.Callbacks;

package body Top_Pkg is

procedure Gtk_New (Top : out Top_Access) is
begin
   Top := new Top_Record;
   Top_Pkg.Initialize (Top);
end Gtk_New;

procedure Initialize (Top : access Top_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Top, Window_Toplevel);
   Set_Title (Top, -"EXPRES - CONTROL DE MISION");
   Set_Position (Top, Win_Pos_None);
   Set_Modal (Top, False);
   Set_Default_Size (Top, 800, 600);

   Gtk_New_Vbox (Top.Vbox1, False, 0);

   Gtk_New (Top.Menubar);

   Gtk_New_With_Mnemonic (Top.Archivo, -("_Archivo"));

   Gtk_New (Top.Menu1);

   Gtk_New_From_Stock (Top.Nuevo1, "gtk-new");

   Image_Menu_Item_Callback.Connect
     (Top.Nuevo1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Nuevo1_Activate'Access), False);
   Append (Top.Menu1, Top.Nuevo1);
   Gtk_New_From_Stock (Top.Abrir1, "gtk-open");

   Image_Menu_Item_Callback.Connect
     (Top.Abrir1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Abrir1_Activate'Access), False);
   Append (Top.Menu1, Top.Abrir1);
   Gtk_New_From_Stock (Top.Guardar1, "gtk-save");

   Image_Menu_Item_Callback.Connect
     (Top.Guardar1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Guardar1_Activate'Access), False);
   Append (Top.Menu1, Top.Guardar1);
   Gtk_New_From_Stock (Top.Guardar_Como1, "gtk-save-as");

   Image_Menu_Item_Callback.Connect
     (Top.Guardar_Como1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Guardar_Como1_Activate'Access), False);
   Append (Top.Menu1, Top.Guardar_Como1);
   Gtk_New (Top.Separatormenuitem1);

   Append (Top.Menu1, Top.Separatormenuitem1);
   Gtk_New_From_Stock (Top.Salir1, "gtk-quit");

   Image_Menu_Item_Callback.Connect
     (Top.Salir1, "activate",
      Image_Menu_Item_Callback.To_Marshaller (On_Salir1_Activate'Access), False);
   Append (Top.Menu1, Top.Salir1);
   Set_Submenu (Top.Archivo, Top.Menu1);
   Append (Top.Menubar, Top.Archivo);
   Gtk_New_With_Mnemonic (Top.Ayuda, -("A_yuda"));

   Gtk_New (Top.Menu4);

   Gtk_New_With_Mnemonic (Top.Acerca_De1, -("_Acerca de"));

   Menu_Item_Callback.Connect
     (Top.Acerca_De1, "activate",
      Menu_Item_Callback.To_Marshaller (On_Acerca_De1_Activate'Access), False);
   Append (Top.Menu4, Top.Acerca_De1);
   Set_Submenu (Top.Ayuda, Top.Menu4);
   Append (Top.Menubar, Top.Ayuda);
   Pack_Start
     (Top.Vbox1,
      Top.Menubar,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New_Hpaned (Top.Divisor);
   Set_Position (Top.Divisor, 0);

   Gtk_New (Top.Scrolledwindow1);
   Set_Policy (Top.Scrolledwindow1, Policy_Always, Policy_Always);
   Set_Shadow_Type (Top.Scrolledwindow1, Shadow_None);

   Gtk_New (Top.Consolas);
   Set_Headers_Visible (Top.Consolas, True);
   Set_Rules_Hint (Top.Consolas, False);
   Set_Reorderable (Top.Consolas, False);
   Set_Enable_Search (Top.Consolas, True);

   Add (Top.Scrolledwindow1, Top.Consolas);
   Pack1 (Top.Divisor, Top.Scrolledwindow1, False, False);
   Gtk_New (Top.Etiqueta_Consolas, -("Consolas de informacion"));
   Set_Alignment (Top.Etiqueta_Consolas, 0.5, 0.5);
   Set_Padding (Top.Etiqueta_Consolas, 0, 0);
   Set_Justify (Top.Etiqueta_Consolas, Justify_Left);
   Set_Line_Wrap (Top.Etiqueta_Consolas, False);
   Set_Selectable (Top.Etiqueta_Consolas, False);
   Set_Use_Markup (Top.Etiqueta_Consolas, False);
   Set_Use_Underline (Top.Etiqueta_Consolas, False);

   Pack2 (Top.Divisor, Top.Etiqueta_Consolas, True, True);
   Pack_Start
     (Top.Vbox1,
      Top.Divisor,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Gtk_New (Top.Statusbar);

   Pack_Start
     (Top.Vbox1,
      Top.Statusbar,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Top, Top.Vbox1);
   Return_Callback.Connect
     (Top, "delete_event", On_Top_Delete_Event'Access, False);
end Initialize;

end Top_Pkg;
