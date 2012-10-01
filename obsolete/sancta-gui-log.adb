with Glib; use Glib;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;         use Gtk.Tree_Model;

package body Sancta.Gui.Log is

   Column_Text : constant := 0;

   ------------
   -- Create --
   ------------

   procedure Create
     (This          : out Object;
      Default_Level : in Agpl.Trace.All_Levels := Agpl.Trace.Informative;
      Console_Echo  : in Boolean    := False;
      File_Name     : in String     := "";
      Active        : in Boolean    := True;
      Clear         : in Boolean    := False)
   is
   begin
      --  Regular creation
      Agpl.Trace.Create (Agpl.Trace.Object (This),
                         Default_Level,
                         Console_Echo,
                         File_Name,
                         Active,
                         Clear);

      --  Widgets creation
      --  Model initialization
      declare
         Types : constant GType_Array := (Column_Text => Gtype_String);
      begin
         Gtk_New (This.Data, Types);
      end;

      --  View initialization
      Gtk_New (This.View, This.Data);

      --  Column initialization
      Gtk_New (This.Col);
      Set_Title (This.Col, "Plan");
      declare
         Cell : Gtk_Cell_Renderer_Text;
      begin
         Gtk_New (Cell);
         Pack_Start (This.Col, Cell, Expand => False);
         Add_Attribute (This.Col, Cell, "text", Column_text);
      end;

      declare
         X : Gint;
         pragma Unreferenced (X);
      begin
         X := Append_Column (This.View, This.Col);
      end;

      --  Scroll:
      Gtk_New (This.Scroll);
      Set_Policy (This.Scroll, Policy_Automatic, Policy_Automatic);
      Add (This.Scroll, This.View);
   end Create;

   ----------------
   -- Custom_Log --
   ----------------

   procedure Custom_Log
     (This    : in out Object;
      Text    : in     String;
      Level   : in     Levels;
      Section : in     String;
      Logged  : in     Boolean;
      Force   :    out Boolean)
   is
      pragma Unreferenced (Level, Section);
   begin
      Force := False;
      if Logged then
         declare
            procedure Log_It is
               I : Gtk_Tree_Iter;
            begin
               Append (This.Data, I, Null_Iter);
               Set (This.Data, I, Column_Text, Text);

               --  If at bottom, scroll down:
               declare
                  Vert : constant Gtk_Adjustment := Get_VAdjustment (This.Scroll);
               begin
                  if Get_Value (Vert) >= Get_Upper (Vert) - Get_Page_Size (Vert) then
                     Set_Value (Vert, Get_Upper (Vert));
                  end if;
               end;
            end Log_It;
         begin
            Log_It;
         end;
      end if;
   end Custom_Log;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Scroll);
   end Get_Widget;

end Sancta.Gui.Log;
