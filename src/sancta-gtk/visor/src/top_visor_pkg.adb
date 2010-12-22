with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Visor; use Callbacks_Visor;
with Visor_Intl; use Visor_Intl;

package body Top_Visor_Pkg is

procedure Gtk_New (Top_Visor : out Top_Visor_Access) is
begin
   Top_Visor := new Top_Visor_Record;
   Top_Visor_Pkg.Initialize (Top_Visor);
end Gtk_New;

procedure Initialize (Top_Visor : access Top_Visor_Record'Class) is
   pragma Suppress (All_Checks);
   Pixmaps_Dir : constant String := "pixmaps/";
begin
   Gtk.Window.Initialize (Top_Visor, Window_Toplevel);
   Set_Title (Top_Visor, -"EXPRES");
   Set_Position (Top_Visor, Win_Pos_None);
   Set_Modal (Top_Visor, False);
   Set_Resizable (Top_Visor, False);

   Gtk_New_Vbox (Top_Visor.Vbox1, False, 0);

   Gtk_New (Top_Visor.Menubar);

   Gtk_New_With_Mnemonic (Top_Visor.Ver, -("_Ver"));

   Append (Top_Visor.Menubar, Top_Visor.Ver);
   Pack_Start
     (Top_Visor.Vbox1,
      Top_Visor.Menubar,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Top_Visor.Stop_Button);
   Set_Relief (Top_Visor.Stop_Button, Relief_Normal);

   Gtk_New
     (Top_Visor.Alignment4, 0.5, 0.5, 0.0, 
      0.0);

   Gtk_New_Hbox (Top_Visor.Hbox4, False, 2);

   Gtk_New (Top_Visor.Image4 , "gtk-stop", Gtk_Icon_Size'Val (4));
   Set_Alignment (Top_Visor.Image4, 0.5, 0.5);
   Set_Padding (Top_Visor.Image4, 0, 0);

   Pack_Start
     (Top_Visor.Hbox4,
      Top_Visor.Image4,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Top_Visor.Label4, -("_Stop!"));
   Set_Alignment (Top_Visor.Label4, 0.5, 0.5);
   Set_Padding (Top_Visor.Label4, 0, 0);
   Set_Justify (Top_Visor.Label4, Justify_Left);
   Set_Line_Wrap (Top_Visor.Label4, False);
   Set_Selectable (Top_Visor.Label4, False);
   Set_Use_Markup (Top_Visor.Label4, False);
   Set_Use_Underline (Top_Visor.Label4, True);

   Pack_Start
     (Top_Visor.Hbox4,
      Top_Visor.Label4,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Top_Visor.Alignment4, Top_Visor.Hbox4);
   Add (Top_Visor.Stop_Button, Top_Visor.Alignment4);
   Set_Size_Request (Top_Visor.Stop_Button, 100, 66);
   Grab_Focus (Top_Visor.Stop_Button);
   Pack_Start
     (Top_Visor.Vbox1,
      Top_Visor.Stop_Button,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New_Hseparator (Top_Visor.Hseparator1);

   Pack_Start
     (Top_Visor.Vbox1,
      Top_Visor.Hseparator1,
      Expand  => False,
      Fill    => False,
      Padding => 8);
   Gtk_New (Top_Visor.Clear_Button);
   Set_Relief (Top_Visor.Clear_Button, Relief_Normal);

   Gtk_New
     (Top_Visor.Alignment3, 0.5, 0.5, 0.0, 
      0.0);

   Gtk_New_Hbox (Top_Visor.Hbox3, False, 2);

   Gtk_New (Top_Visor.Image3 , "gtk-clear", Gtk_Icon_Size'Val (4));
   Set_Alignment (Top_Visor.Image3, 0.5, 0.5);
   Set_Padding (Top_Visor.Image3, 0, 0);

   Pack_Start
     (Top_Visor.Hbox3,
      Top_Visor.Image3,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Top_Visor.Label3, -("_Clear"));
   Set_Alignment (Top_Visor.Label3, 0.5, 0.5);
   Set_Padding (Top_Visor.Label3, 0, 0);
   Set_Justify (Top_Visor.Label3, Justify_Left);
   Set_Line_Wrap (Top_Visor.Label3, False);
   Set_Selectable (Top_Visor.Label3, False);
   Set_Use_Markup (Top_Visor.Label3, False);
   Set_Use_Underline (Top_Visor.Label3, True);

   Pack_Start
     (Top_Visor.Hbox3,
      Top_Visor.Label3,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Top_Visor.Alignment3, Top_Visor.Hbox3);
   Add (Top_Visor.Clear_Button, Top_Visor.Alignment3);
   Pack_Start
     (Top_Visor.Vbox1,
      Top_Visor.Clear_Button,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Top_Visor.Record_Button);
   Set_Relief (Top_Visor.Record_Button, Relief_Normal);
   Set_Active (Top_Visor.Record_Button, False);
   Set_Inconsistent (Top_Visor.Record_Button, False);
   Set_Relief (Top_Visor.Record_Button, Relief_Normal);

   Gtk_New
     (Top_Visor.Alignment2, 0.5, 0.5, 0.0, 
      0.0);

   Gtk_New_Hbox (Top_Visor.Hbox2, False, 2);

   Gtk_New (Top_Visor.Image2 , "gtk-media-record", Gtk_Icon_Size'Val (4));
   Set_Alignment (Top_Visor.Image2, 0.5, 0.5);
   Set_Padding (Top_Visor.Image2, 0, 0);

   Pack_Start
     (Top_Visor.Hbox2,
      Top_Visor.Image2,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Gtk_New (Top_Visor.Label2, "_Record");
   Set_Alignment (Top_Visor.Label2, 0.5, 0.5);
   Set_Padding (Top_Visor.Label2, 0, 0);
   Set_Justify (Top_Visor.Label2, Justify_Left);
   Set_Line_Wrap (Top_Visor.Label2, False);
   Set_Selectable (Top_Visor.Label2, False);
   Set_Use_Markup (Top_Visor.Label2, False);
   Set_Use_Underline (Top_Visor.Label2, True);

   Pack_Start
     (Top_Visor.Hbox2,
      Top_Visor.Label2,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Top_Visor.Alignment2, Top_Visor.Hbox2);
   Add (Top_Visor.Record_Button, Top_Visor.Alignment2);
   Pack_Start
     (Top_Visor.Vbox1,
      Top_Visor.Record_Button,
      Expand  => False,
      Fill    => False,
      Padding => 0);
   Add (Top_Visor, Top_Visor.Vbox1);
end Initialize;

end Top_Visor_Pkg;
