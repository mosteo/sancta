with Agpl.Calendar.Format;
with Sancta.Plan_Node;
with Sancta.Tasks;
with Agpl.Strings;        use Agpl.Strings;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Glib; use Glib;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

package body Sancta.Gui.Plan is

   Column_Mix   : constant := 0;
   Column_Owner : constant := 1;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Scroll);
   end Get_Widget;

   --------------
   -- Set_Plan --
   --------------

   procedure Set_Plan
     (This     : in out Object;
      New_Plan : Sancta.Plan.Object)
   is
      Plan : Sancta.Plan.Object renames New_Plan;

      I    : Gtk_Tree_Iter;
      use Sancta.Plan_Node;

      --------------
      -- Add_Node --
      --------------

      procedure Add_Node (X : Node_Access; Parent : Gtk_Tree_Iter) is
         Text : Ustring;
         I    : Gtk_Tree_Iter;
         use ASU;
         Own  : Ustring;
      begin
         if X = null then
            return;
         end if;

         case Get_Kind (X) is
            when Task_Node =>
               declare
                  T : Sancta.Tasks.Object renames Get_Task (X).all;
               begin
                  Append (Text,
                          "#" & Text & To_String (Integer (Sancta.Tasks.Get_Id (T))) &
                          " - " & Sancta.Tasks.To_String (Get_Task (X).all)
                         --  & " (" & Ada.Tags.External_Tag (T'Tag) & ")"
                         );
                  if Get_Finished (X) then
                     Append (Text, " finished");
                  end if;
               end;
               Own := + Sancta.Plan_Node.Get_Owner (X);
            when And_Node =>
               ASU.Append (Text, "AND");
            when Or_Node =>
               ASU.Append (Text, "OR");
         end case;

         Append (This.Data, I, Parent);
         Set (This.Data, I, Column_Mix, S (Text));
         Set (This.Data, I, Column_Owner, + Own);

         case Get_Kind (X) is
            when Task_Node =>
               Add_Node (Get_Expansion (X), I);
            when And_Node | Or_Node =>
               declare
                  procedure Add_Children (X : Node_Lists.Cursor) is
                  begin
                     Add_Node (Node_Lists.Element (X), I);
                  end Add_Children;
                  Children : constant Node_Lists.List := Get_Children (X);
               begin
                  Node_Lists.Iterate (Children, Add_Children'Access);
               end;
         end case;
      end Add_Node;

   begin
      --      Clear (This.Data);
      This.Num := This.Num + 1;

      Insert (This.Data, I, Null_Iter, 0);
      Set (This.Data, I, Column_Mix,
           "Plan #" & To_String (This.Num) &
           " at " & Agpl.Calendar.Format.Timestamp);
      Add_Node (Sancta.Plan.Get_Root (Plan), I);

      --  Close all plans.
      Collapse_All (This.View);

      --  And open the current one.
      declare
         P : constant Gtk_Tree_Path := Gtk_New ("0");
      begin
         if Expand_Row (This.View, P, True) then null; end if;
         Path_Free (P);
      end;
   end Set_Plan;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      Col  : Gtk_Tree_View_Column;
   begin
      --  Model initialization
      declare
         Types : constant GType_Array := (Column_Mix   => Gtype_String,
                                          Column_Owner => Gtype_String);
      begin
         Gtk_New (This.Data, Types);
      end;

      --  View initialization
      Gtk_New (This.View, This.Data);

      --  Columns initialization
      Gtk_New (Col);
      Set_Title (Col, "Plan");
      declare
         Cell : Gtk_Cell_Renderer_Text;
      begin
         Gtk_New (Cell);
         Pack_Start (Col, Cell, Expand => False);
         Add_Attribute (Col, Cell, "text", Column_Mix);
      end;

      declare
         X : Gint;
         pragma Unreferenced (X);
      begin
         X := Append_Column (This.View, Col);
      end;

      --  Owner column
      Gtk_New (Col);
      Set_Title (Col, "Owner");
      declare
         Cell : Gtk_Cell_Renderer_Text;
      begin
         Gtk_New (Cell);
         Pack_Start (Col, Cell, Expand => False);
         Add_Attribute (Col, Cell, "text", Column_Owner);
      end;

      declare
         X : Gint;
         pragma Unreferenced (X);
      begin
         X := Append_Column (This.View, Col);
      end;

      --  Scroll:
      Gtk_New (This.Scroll);
      Set_Policy (This.Scroll, Policy_Automatic, Policy_Automatic);
      Add (This.Scroll, This.View);
   end Initialize;

   ------------------
   -- Plan_Changed --
   ------------------

   procedure Plan_Changed (This : in out Synch; New_Plan : Sancta.Plan.Object) is
   begin
      This.Parent.Set_Plan (New_Plan);
   end Plan_Changed;

end Sancta.Gui.Plan;
