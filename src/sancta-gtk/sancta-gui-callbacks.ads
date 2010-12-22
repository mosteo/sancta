with Sancta.Gui.Visor;
with Sancta.Gui.User_Data_Handle;
with Sancta.Tasks.Types;

with Gdk.Event;
with Gtk.Handlers;
with Gtk.Tree_Selection;
with Gtk.Widget; use Gtk.Widget;

package Sancta.Gui.Callbacks is

   --  Instantiations for widget connections:

   package Handlers is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      User_Data_Handle.Object);

   package Handlers_Pursuit is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Tasks.Types.Smart_Pursuit_Info.Object);

   procedure Console_Changed
     (Widget : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class);
   --  When the user changes the console to display.

   function Expose_Gaps (Widget : access Gtk_Widget_Record'Class;
                         Event  :        Gdk.Event.Gdk_Event_Expose;
                         Data   :        User_Data_Handle.Object)
                         return          Boolean;
   --  Drawing of gaps being seen by some andoba.

   function Pursuit_Click (Widget : access Gtk_Widget_Record'Class;
                           Event  :        Gdk.Event.Gdk_Event_Expose;
                           Data   :        Tasks.Types.Smart_Pursuit_Info.Object)
                           return          Boolean;
   --  Used to handle "button_press_event"

   function Pursuit_Expose (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose;
                            Data   :        Tasks.Types.Smart_Pursuit_Info.Object)
                            return          Boolean;
   --  Drawing of pursuit task info.

   function User_Click (Widget : access Gtk_Widget_Record'Class;
                        Event  :        Gdk.Event.Gdk_Event_Expose;
                        Data   :        User_Data_Handle.Object)
                        return          Boolean;
   --  Used to handle "button_press_event"

   function User_Data_Draw (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose;
                            Data   :        User_Data_Handle.Object)
                            return          Boolean;
   --  Drawing of any user data. Will discriminate by the class of the object.

end Sancta.Gui.Callbacks;
