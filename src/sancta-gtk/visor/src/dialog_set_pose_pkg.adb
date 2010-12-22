with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Setpose; use Callbacks_Setpose;
with Setpose_Intl; use Setpose_Intl;

package body Dialog_Set_Pose_Pkg is

procedure Gtk_New (Dialog_Set_Pose : out Dialog_Set_Pose_Access) is
begin
   Dialog_Set_Pose := new Dialog_Set_Pose_Record;
   Dialog_Set_Pose_Pkg.Initialize (Dialog_Set_Pose);
end Gtk_New;

procedure Initialize (Dialog_Set_Pose : access Dialog_Set_Pose_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Dialog_Set_Pose, Window_Toplevel);
   Set_Title (Dialog_Set_Pose, -"Set Pose");
   Set_Position (Dialog_Set_Pose, Win_Pos_None);
   Set_Modal (Dialog_Set_Pose, True);
   Set_Resizable (Dialog_Set_Pose, False);

   Gtk_New_Vbox (Dialog_Set_Pose.Vbox1, False, 0);

   Gtk_New (Dialog_Set_Pose.Table1, 3, 2, False);
   Set_Row_Spacings (Dialog_Set_Pose.Table1, 0);
   Set_Col_Spacings (Dialog_Set_Pose.Table1, 0);

   Gtk_New (Dialog_Set_Pose.Entry_X);
   Set_Editable (Dialog_Set_Pose.Entry_X, True);
   Set_Max_Length (Dialog_Set_Pose.Entry_X, 0);
   Set_Text (Dialog_Set_Pose.Entry_X, -("0.0"));
   Set_Visibility (Dialog_Set_Pose.Entry_X, True);
   Set_Invisible_Char (Dialog_Set_Pose.Entry_X, UTF8_Get_Char ("●"));

   Grab_Focus (Dialog_Set_Pose.Entry_X);
   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Entry_X,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Pose.Entry_Y);
   Set_Editable (Dialog_Set_Pose.Entry_Y, True);
   Set_Max_Length (Dialog_Set_Pose.Entry_Y, 0);
   Set_Text (Dialog_Set_Pose.Entry_Y, -("0.0"));
   Set_Visibility (Dialog_Set_Pose.Entry_Y, True);
   Set_Invisible_Char (Dialog_Set_Pose.Entry_Y, UTF8_Get_Char ("●"));

   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Entry_Y,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Pose.Entry_A);
   Set_Editable (Dialog_Set_Pose.Entry_A, True);
   Set_Max_Length (Dialog_Set_Pose.Entry_A, 0);
   Set_Text (Dialog_Set_Pose.Entry_A, -("0.0"));
   Set_Visibility (Dialog_Set_Pose.Entry_A, True);
   Set_Invisible_Char (Dialog_Set_Pose.Entry_A, UTF8_Get_Char ("●"));

   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Entry_A,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 2,
      Bottom_Attach  => 3,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Pose.Label6, -("X: "));
   Set_Alignment (Dialog_Set_Pose.Label6, 0.0, 0.5);
   Set_Padding (Dialog_Set_Pose.Label6, 0, 0);
   Set_Justify (Dialog_Set_Pose.Label6, Justify_Right);
   Set_Line_Wrap (Dialog_Set_Pose.Label6, False);
   Set_Selectable (Dialog_Set_Pose.Label6, False);
   Set_Use_Markup (Dialog_Set_Pose.Label6, False);
   Set_Use_Underline (Dialog_Set_Pose.Label6, False);

   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Label6,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Pose.Label7, -("Y: "));
   Set_Alignment (Dialog_Set_Pose.Label7, 0.0, 0.5);
   Set_Padding (Dialog_Set_Pose.Label7, 0, 0);
   Set_Justify (Dialog_Set_Pose.Label7, Justify_Right);
   Set_Line_Wrap (Dialog_Set_Pose.Label7, False);
   Set_Selectable (Dialog_Set_Pose.Label7, False);
   Set_Use_Markup (Dialog_Set_Pose.Label7, False);
   Set_Use_Underline (Dialog_Set_Pose.Label7, False);

   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Label7,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Pose.Label8, -("A: "));
   Set_Alignment (Dialog_Set_Pose.Label8, 0.0, 0.5);
   Set_Padding (Dialog_Set_Pose.Label8, 0, 0);
   Set_Justify (Dialog_Set_Pose.Label8, Justify_Right);
   Set_Line_Wrap (Dialog_Set_Pose.Label8, False);
   Set_Selectable (Dialog_Set_Pose.Label8, False);
   Set_Use_Markup (Dialog_Set_Pose.Label8, False);
   Set_Use_Underline (Dialog_Set_Pose.Label8, False);

   Attach
     (Dialog_Set_Pose.Table1,
       Dialog_Set_Pose.Label8,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 2,
      Bottom_Attach  => 3,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Pack_Start
     (Dialog_Set_Pose.Vbox1,
      Dialog_Set_Pose.Table1,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Gtk_New_From_Stock (Dialog_Set_Pose.Button_Ok, "gtk-ok");
   Set_Relief (Dialog_Set_Pose.Button_Ok, Relief_Normal);

   Set_Flags (Dialog_Set_Pose.Button_Ok, Can_Default);
   Grab_Default (Dialog_Set_Pose.Button_Ok);
   Pack_Start
     (Dialog_Set_Pose.Vbox1,
      Dialog_Set_Pose.Button_Ok,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Dialog_Set_Pose, Dialog_Set_Pose.Vbox1);
end Initialize;

end Dialog_Set_Pose_Pkg;
