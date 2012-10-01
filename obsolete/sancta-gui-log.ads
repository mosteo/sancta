--  To display arbitrary debug logs.

with Agpl.Trace;

with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Widget;           use Gtk.Widget;

package Sancta.Gui.Log is

   pragma Elaborate_Body;

   type Object is new Agpl.Trace.Object with private;

   subtype Levels is Agpl.Trace.Levels;

   procedure Create
     (This          : out Object;
      Default_Level : in Agpl.Trace.All_Levels := Agpl.Trace.Informative;
      Console_Echo  : in Boolean    := False;
      File_Name     : in String     := "";
      Active        : in Boolean    := True;
      Clear         : in Boolean    := False);

   procedure Custom_Log
     (This    : in out Object;
      Text    : in     String;
      Level   : in     Levels;
      Section : in     String;
      Logged  : in     Boolean;
      Force   :    out Boolean);
   --  Our logging procedure.

   function Get_Widget (This : Object) return Gtk_Widget;
   --  Get the widget so it could be displayed.

private

   type Object is new Agpl.Trace.Object with
      record
         Data     : Gtk_Tree_Store;
         View     : Gtk_Tree_View;
         Col      : Gtk_Tree_View_Column;
         Scroll   : Gtk_Scrolled_Window;
      end record;

end Sancta.Gui.Log;
