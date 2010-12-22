with Sancta.Gui.Visor_Factory; pragma Elaborate_All (Sancta.Gui.Visor_Factory);

with Glib; use Glib;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;

with Agpl.Trace; use Agpl.Trace;

with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Gui.Visor_Log_View is

   ------------
   -- Create --
   ------------

   function Create (Context : Visor_Factory.Creation_Context)
                    return    Visor_Widget.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      Gtk_New (This.Scroll);
      Set_Policy (This.Scroll, Policy_Automatic, Policy_Automatic);
      Set_Shadow_Type (This.Scroll, Shadow_In);

      --  We defer proper creation until the Set_Data call.

      return Visor_Widget.Object_Access (This);
   exception
      when E : others =>
         Put_Line ("Visor_Log_View.Create: " & Report (E));
         Log ("Visor_Log_View.Create: " & Report (E), Error);
         return null;
   end Create;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : in Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Scroll);
   end Get_Widget;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object)
   is
      procedure Add_Col (Title : in String; Pos : in Gint) is
         Col  : Gtk_Tree_View_Column;
         X    : Gint; pragma Unreferenced (X);
      begin
         Gtk_New (Col);
         Set_Title (Col, Title);
         declare
            Cell : Gtk_Cell_Renderer_Text;
         begin
            Gtk_New (Cell);
            Pack_Start (Col, Cell, Expand => False);
            Add_Attribute (Col, Cell, "text", Pos);
         end;
         X := Append_Column (This.Log_Tree, Col);
      end Add_Col;
   begin
      This.Data := Visor_Data.Object_Access (Data);

      --  View initialization
      Gtk_New (This.Log_Tree, Gtk_Tree_Model (Visor_Data.Get_Logs (This.Data.all)));

      --  Columns initialization
      Add_Col ("Sender",    Visor_Data.Sender_Column);
      Add_Col ("Timestamp", Visor_Data.Date_Column);
      Add_Col ("Severity",  Visor_Data.Level_Column);
      Add_Col ("Section",   Visor_Data.Section_Column);
      Add_Col ("Text",      Visor_Data.Text_Column);

      Add (This.Scroll, This.Log_Tree);
   end Set_Data;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Object;
                     Id   : in     Node_Id;
                     Msg  : in     Robot_Data.Network_Update)
   is
      pragma Unreferenced (This, Id, Msg);
      --  Vert : constant Gtk_Adjustment := Get_VAdjustment (This.Scroll);
   begin
      --  Scroll to bottom if necessary:
--        if Get_Value (Vert) >= Get_Upper (Vert) - Get_Page_Size (Vert) then
--           Set_Value (Vert, Get_Upper (Vert));
--        end if;
      null;
   end Update;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Visor_Factory.Register (Create'Access, "Log view");
   end Register;

end Sancta.Gui.Visor_Log_View;
