with Sancta.Gui.Callbacks;

with Top_Pkg;

with Glib;
with Gtk.Cell_Renderer_Text;
with Gtk.Container;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Paned;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with Ada.Containers.Vectors;

package body Sancta.Gui.Main is

   --  Constants for things in the gui
   COLUMN_CONSOLE : constant := 0;

   package Widget_Vectors is new
     Ada.Containers.Vectors (Natural, Gtk.Widget.Gtk_Widget, Gtk.Widget."=");

   package Tree_Selection_Callback is new
     Gtk.Handlers.Callback (Gtk.Tree_Selection.Gtk_Tree_Selection_Record);

   -------------------
   -- Gui variables --
   -------------------

   Top   : Top_Pkg.Top_Access;
   Col   : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   Model : Gtk.Tree_Store.Gtk_Tree_Store;
   Text  : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;

   Consoles : Widget_Vectors.Vector;

   ----------------
   -- Add_Canvas --
   ----------------
   procedure Add_Canvas (C : in Canvas.Object) is
      use Gtk.Tree_Model;
      use Gtk.Tree_Store;
      I : Gtk_Tree_Iter;
   begin
      Append (Model, I, Null_Iter);
      Set    (Model, I, 0, Canvas.Get_Name (C));
      --  Add a ref so when switching panes it will persist:
      Gtk.Widget.Ref (Canvas.Get_Widget (C));
      Widget_Vectors.Append (Consoles, Canvas.Get_Widget (C));
   end Add_Canvas;

   -------------
   -- Connect --
   -------------

   procedure Connect is
      use Gtk.Tree_View;
      use Tree_Selection_Callback;
   begin
      Connect
        (Get_Selection (Top.Consolas),
         "changed",
         To_Marshaller (Gui.Callbacks.Console_Changed'Access));
   end Connect;

   --------------------
   -- Display_Canvas --
   --------------------

   procedure Display_Canvas (I : in Natural) is
      use Gtk.Container;
      use Gtk.Paned;
      use Gtk.Widget;
   begin
      Remove   (Top.Divisor, Get_Child2 (Top.Divisor));
      Pack2    (Top.Divisor, Widget_Vectors.Element (Consoles, I), True);
      Show_All (Widget_Vectors.Element (Consoles, I));
   end Display_Canvas;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Gtk.Main.Set_Locale;
      Gtk.Main.Init;

      Top_Pkg.Gtk_New (Top);
      Top_Pkg.Show_All (Top);

      --  Model for the Tree_View
      Gtk.Tree_Store.Gtk_New
        (Model,
         (COLUMN_CONSOLE => Glib.Gtype_String));

      Gtk.Tree_View.Set_Model
        (Top.Consolas, Gtk.Tree_Model.Gtk_Tree_Model (Model));

      --  Prepare the console column
      declare
         use Gtk.Cell_Renderer_Text;
         use Gtk.Tree_View_Column;
         Cols : Glib.Gint;
         pragma Unreferenced (Cols);
      begin
         Gtk_New (Text);
         Gtk_New (Col);
         Set_Title (Col, "Consola");

         Pack_Start (Col, Text, Expand => False);
         Add_Attribute (Col, Text, "text", COLUMN_CONSOLE);

         Cols := Gtk.Tree_View.Append_Column (Top.Consolas, Col);
      end;

      --  Do connections
      Connect;
   end Start;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      while Gtk.Main.Events_Pending loop
         if Gtk.Main.Main_Iteration then
            null;
         end if;
      end loop;
   end Update;

end Sancta.Gui.Main;
