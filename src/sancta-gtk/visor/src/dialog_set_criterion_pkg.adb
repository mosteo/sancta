with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Setcriteria; use Callbacks_Setcriteria;
with Setcriteria_Intl; use Setcriteria_Intl;

package body Dialog_Set_Criterion_Pkg is

procedure Gtk_New (Dialog_Set_Criterion : out Dialog_Set_Criterion_Access) is
begin
   Dialog_Set_Criterion := new Dialog_Set_Criterion_Record;
   Dialog_Set_Criterion_Pkg.Initialize (Dialog_Set_Criterion);
end Gtk_New;

procedure Initialize (Dialog_Set_Criterion : access Dialog_Set_Criterion_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Dialog_Set_Criterion, Window_Toplevel);
   Set_Title (Dialog_Set_Criterion, -"Set Criterion");
   Set_Position (Dialog_Set_Criterion, Win_Pos_None);
   Set_Modal (Dialog_Set_Criterion, True);
   Set_Resizable (Dialog_Set_Criterion, False);

   Gtk_New_Vbox (Dialog_Set_Criterion.Vbox1, False, 0);

   Gtk_New (Dialog_Set_Criterion.Table1, 2, 2, False);
   Set_Row_Spacings (Dialog_Set_Criterion.Table1, 0);
   Set_Col_Spacings (Dialog_Set_Criterion.Table1, 0);

   Gtk_New (Dialog_Set_Criterion.Label1, -("MinMax:"));
   Set_Alignment (Dialog_Set_Criterion.Label1, 0.0, 0.5);
   Set_Padding (Dialog_Set_Criterion.Label1, 0, 0);
   Set_Justify (Dialog_Set_Criterion.Label1, Justify_Right);
   Set_Line_Wrap (Dialog_Set_Criterion.Label1, False);
   Set_Selectable (Dialog_Set_Criterion.Label1, False);
   Set_Use_Markup (Dialog_Set_Criterion.Label1, False);
   Set_Use_Underline (Dialog_Set_Criterion.Label1, False);

   Attach
     (Dialog_Set_Criterion.Table1,
       Dialog_Set_Criterion.Label1,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Criterion.Label2, -("MinSum:"));
   Set_Alignment (Dialog_Set_Criterion.Label2, 0.0, 0.5);
   Set_Padding (Dialog_Set_Criterion.Label2, 0, 0);
   Set_Justify (Dialog_Set_Criterion.Label2, Justify_Right);
   Set_Line_Wrap (Dialog_Set_Criterion.Label2, False);
   Set_Selectable (Dialog_Set_Criterion.Label2, False);
   Set_Use_Markup (Dialog_Set_Criterion.Label2, False);
   Set_Use_Underline (Dialog_Set_Criterion.Label2, False);

   Attach
     (Dialog_Set_Criterion.Table1,
       Dialog_Set_Criterion.Label2,      Left_Attach  => 0,
      Right_Attach  => 1,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xoptions  => Fill,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Criterion.Minmax);
   Set_Editable (Dialog_Set_Criterion.Minmax, True);
   Set_Max_Length (Dialog_Set_Criterion.Minmax, 0);
   Set_Text (Dialog_Set_Criterion.Minmax, -(""));
   Set_Visibility (Dialog_Set_Criterion.Minmax, True);
   Set_Invisible_Char (Dialog_Set_Criterion.Minmax, UTF8_Get_Char ("●"));

   Grab_Focus (Dialog_Set_Criterion.Minmax);
   Attach
     (Dialog_Set_Criterion.Table1,
       Dialog_Set_Criterion.Minmax,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 0,
      Bottom_Attach  => 1,
      Xpadding  => 0,
      Ypadding  => 0);
   Gtk_New (Dialog_Set_Criterion.Minsum);
   Set_Editable (Dialog_Set_Criterion.Minsum, True);
   Set_Max_Length (Dialog_Set_Criterion.Minsum, 0);
   Set_Text (Dialog_Set_Criterion.Minsum, -(""));
   Set_Visibility (Dialog_Set_Criterion.Minsum, True);
   Set_Invisible_Char (Dialog_Set_Criterion.Minsum, UTF8_Get_Char ("●"));

   Attach
     (Dialog_Set_Criterion.Table1,
       Dialog_Set_Criterion.Minsum,      Left_Attach  => 1,
      Right_Attach  => 2,
      Top_Attach  => 1,
      Bottom_Attach  => 2,
      Xpadding  => 0,
      Ypadding  => 0);
   Pack_Start
     (Dialog_Set_Criterion.Vbox1,
      Dialog_Set_Criterion.Table1,
      Expand  => True,
      Fill    => True,
      Padding => 0);
   Gtk_New (Dialog_Set_Criterion.Button_Ok);
   Set_Relief (Dialog_Set_Criterion.Button_Ok, Relief_Normal);

   Gtk_New
     (Dialog_Set_Criterion.Alignment1, 0.5, 0.5, 0.0, 
      0.0);

   Gtk_New_Hbox (Dialog_Set_Criterion.Hbox1, False, 2);

   Gtk_New (Dialog_Set_Criterion.Image1 , "gtk-ok", Gtk_Icon_Size'Val (4));
   Set_Alignment (Dialog_Set_Criterion.Image1, 0.5, 0.5);
   Set_Padding (Dialog_Set_Criterion.Image1, 0, 0);

   Pack_Start
     (Dialog_Set_Criterion.Hbox1,
      Dialog_Set_Criterion.Image1,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Dialog_Set_Criterion.Label3, -("Ok"));
   Set_Alignment (Dialog_Set_Criterion.Label3, 0.5, 0.5);
   Set_Padding (Dialog_Set_Criterion.Label3, 0, 0);
   Set_Justify (Dialog_Set_Criterion.Label3, Justify_Left);
   Set_Line_Wrap (Dialog_Set_Criterion.Label3, False);
   Set_Selectable (Dialog_Set_Criterion.Label3, False);
   Set_Use_Markup (Dialog_Set_Criterion.Label3, False);
   Set_Use_Underline (Dialog_Set_Criterion.Label3, True);

   Pack_Start
     (Dialog_Set_Criterion.Hbox1,
      Dialog_Set_Criterion.Label3,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Dialog_Set_Criterion.Alignment1, Dialog_Set_Criterion.Hbox1);
   Add (Dialog_Set_Criterion.Button_Ok, Dialog_Set_Criterion.Alignment1);
   Set_Flags (Dialog_Set_Criterion.Button_Ok, Can_Default);
   Grab_Default (Dialog_Set_Criterion.Button_Ok);
   Pack_Start
     (Dialog_Set_Criterion.Vbox1,
      Dialog_Set_Criterion.Button_Ok,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Dialog_Set_Criterion, Dialog_Set_Criterion.Vbox1);
end Initialize;

end Dialog_Set_Criterion_Pkg;
