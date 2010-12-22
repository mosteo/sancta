with Sancta.Distributed;
with Sancta.Distributed.Datastore;
with Sancta.Gui.Visor_Factory; pragma Elaborate_All (Sancta.Gui.Visor_Factory);

with Glib; use Glib;
--  with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;

with Agpl.Chronos;
with Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

------------------------------
-- Sancta.Gui.Visor_Db_View --
------------------------------

package body Sancta.Gui.Visor_Db_View is

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
         Put_Line ("Visor_Db_View.Create: " & Report (E));
         Log ("Visor_Db_View.Create: " & Report (E), Error);
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
      pragma Unreferenced (Data);
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
      Gtk_New (This.Data, (Visor_Data.First_Column ..
                           Visor_Data.First_Column + 1 => Gtype_String));

      --  View initialization
      Gtk_New (This.Log_Tree,
               This.Data);

      --  Columns initialization
      Add_Col ("Key",   Visor_Data.First_Column);
      Add_Col ("Value", Visor_Data.First_Column + 1);

      Add (This.Scroll, This.Log_Tree);
   end Set_Data;

   ------------
   -- Update --
   ------------

   Update_Cron : Chronos.Object;

   procedure Update (This : in out Object;
                     Id   : in     Node_Id;
                     Msg  : in     Robot_Data.Network_Update)
   is
      pragma Unreferenced (Id, Msg);
      --  Vert : constant Gtk_Adjustment := Get_VAdjustment (This.Scroll);
   begin
      --  Scroll to bottom if necessary:
--        if Get_Value (Vert) >= Get_Upper (Vert) - Get_Page_Size (Vert) then
--           Set_Value (Vert, Get_Upper (Vert));
--        end if;

      --  We'll update here the database
      if Update_Cron.Elapsed < 0.5 then
         return;
      end if;

      Update_Cron.Reset;

      Clear (This.Data);

      declare
         Database : constant Distributed.Datastore.Object_Access := null;
--                      Sancta.Plugin.Shared_Database.Get;
         use Distributed.Datastore;
      begin
         if Database /= null then
            declare
               Values   : constant Distributed.Object_Maps.Map :=
                            Database.Dump;
               procedure Add (I : Distributed.Object_Maps.Cursor) is
                  use Distributed.Object_Maps;
                  TI     : Gtk_Tree_Iter;
               begin
                  Append (This.Data, TI, Null_Iter);
                  Set (This.Data, TI, Visor_Data.First_Column,
                       Distributed.Image (Key (I)));
                  Set (This.Data, TI, Visor_Data.First_Column + 1,
                       Element (I).Image);
               end Add;
            begin
               Values.Iterate (Add'Access);
            end;
         end if;
      end;
   end Update;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Visor_Factory.Register (Create'Access, "Shared database view");
   end Register;

end Sancta.Gui.Visor_Db_View;
