
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Agpl; use Agpl;

with Glib; use Glib;
with Glib.Convert;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

package body Sancta.Gui.Robot is

   function "+" (S : String) return UTF8_String renames
     Glib.Convert.Locale_To_UTF8;

   ------------
   -- Create --
   ------------

   procedure Create (This : out Object; Agent : in out Sancta.Robot.Object) is

      ----------------------
      -- Create_Task_View --
      ----------------------

      procedure Create_Task_View is
         Col : Gtk_Tree_View_Column;
      begin
         --  Model initialization
         declare
            Types : constant GType_Array := (0 => Gtype_String);
         begin
            Gtk_New (This.Task_Data, Types);
         end;

         --  View initialization
         Gtk_New (This.Task_View, This.Task_Data);

         --  Column initialization
         Gtk_New (Col);
         Set_Title (Col, "Tasks");
         declare
            Cell : Gtk_Cell_Renderer_Text;
         begin
            Gtk_New (Cell);
            Pack_Start (Col, Cell, Expand => False);
            Add_Attribute (Col, Cell, "text", 0);
         end;

         declare
            X : Gint;
            pragma Unreferenced (X);
         begin
            X := Append_Column (This.Task_View, Col);
         end;

         --  Scroll for tasks:
         Gtk_New (This.Task_Scroll);
         Set_Policy (This.Task_Scroll, Policy_Automatic, Policy_Automatic);
         Add (This.Task_Scroll, This.Task_View);
      end Create_Task_View;

      procedure Gtk_Create is
      begin
         --  Create the paned window
         Gtk_New_Hpaned (This.Pane);

         --  Create the tree list of tasks
         Create_Task_View;
         Pack2 (This.Pane, This.Task_Scroll);

         --  Left pane
         Gtk_New_VBox (This.Left_Box);
         Pack1 (This.Pane, This.Left_Box);

         Gtk_New (This.Last_Command, +"No command");
         Set_Justify (This.Last_Command, Justify_Left);
         Set_Alignment (This.Last_Command, 0.0, 0.5);
         Pack_End (This.Left_Box, This.Last_Command, Expand => False);

         Gtk_New (This.Localized_Pose, +"Unknown pose");
         Set_Justify (This.Localized_Pose, Justify_Left);
         Set_Alignment (This.Localized_Pose, 0.0, 0.5);
         Pack_End (This.Left_Box, This.Localized_Pose, Expand => False);
      end Gtk_Create;

   begin
      --  Gtk things:
      Gtk_Create;

      --  Register as a listener:
      Sancta.Robot.Add_Listener (Agent, This);
   end Create;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Pane);
   end Get_Widget;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (This : in out Object;
      Name : in     Robot_Data.Signals;
      Data : in     Robot_Data.Object_Access)
   is

      --------------------
      -- Update_Command --
      --------------------

      procedure Update_Command is
      begin
         Set_Text (This.Last_Command,
                   "Last Command: " &
                   Data.Get_Attribute (Robot_Data.Last_Command));
      end Update_Command;

      -----------------
      -- Update_Pose --
      -----------------

      procedure Update_Pose is
      begin
         Set_Text (This.Localized_Pose,
                   "Localized pose: " & Data.Get_Attribute (Robot_Data.Localized_Pose));
      end Update_Pose;

      ------------------
      -- Update_Tasks --
      ------------------

      procedure Update_Tasks is
         T : constant Sancta.Tasks.Containers.Lists.List := Data.Get_Tasks;
         procedure Add_Task (I : Sancta.Tasks.Containers.Cursor) is
            Ta : Sancta.Tasks.Object'Class renames Sancta.Tasks.Containers.Element (I);
            X  : Gtk_Tree_Iter;
         begin
            Append (This.Task_Data, X, Null_Iter);
            Set (This.Task_Data, X, 0, Sancta.Tasks.To_String (Ta));
         end Add_Task;
      begin
         Clear (This.Task_Data);
         Sancta.Tasks.Containers.Iterate (T, Add_Task'Access);
      end Update_Tasks;

      S : constant Robot_Data.Signals := Name;
   begin
      case S is
         when Robot_Data.Command =>
            Update_Command;
         when Robot_Data.Pose =>
            Update_Pose;
         when Robot_Data.Tasks =>
            Update_Tasks;
         when others =>
            null;
      end case;
   end Signal;

end Sancta.Gui.Robot;
