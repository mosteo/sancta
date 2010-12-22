with Gtk.Window; use Gtk.Window;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Button; use Gtk.Button;
package Log_Window_Pkg is

   type Log_Window_Record is new Gtk_Window_Record with record
      Scroll : Gtk_Scrolled_Window;
      Log_Tree : Gtk_Tree_View;
   end record;
   type Log_Window_Access is access Log_Window_Record'Class;

   procedure Gtk_New (Log_Window : out Log_Window_Access);
   procedure Initialize (Log_Window : access Log_Window_Record'Class);

end Log_Window_Pkg;
