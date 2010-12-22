with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Log_Window; use Callbacks_Log_Window;
with Log_Window_Intl; use Log_Window_Intl;

package body Log_Window_Pkg is

procedure Gtk_New (Log_Window : out Log_Window_Access) is
begin
   Log_Window := new Log_Window_Record;
   Log_Window_Pkg.Initialize (Log_Window);
end Gtk_New;

procedure Initialize (Log_Window : access Log_Window_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Log_Window, Window_Toplevel);
   Set_Title (Log_Window, -"Event Log");
   Set_Position (Log_Window, Win_Pos_None);
   Set_Modal (Log_Window, False);

   Gtk_New (Log_Window.Scroll);
   Set_Policy (Log_Window.Scroll, Policy_Always, Policy_Always);
   Set_Shadow_Type (Log_Window.Scroll, Shadow_In);

   Gtk_New (Log_Window.Log_Tree);
   Set_Headers_Visible (Log_Window.Log_Tree, True);
   Set_Rules_Hint (Log_Window.Log_Tree, False);
   Set_Reorderable (Log_Window.Log_Tree, False);
   Set_Enable_Search (Log_Window.Log_Tree, True);

   Add (Log_Window.Scroll, Log_Window.Log_Tree);
   Add (Log_Window, Log_Window.Scroll);
end Initialize;

end Log_Window_Pkg;
