with Sancta.Gui.Config;
with Sancta.Gui.Robot_Data;
with Sancta.Gui.Visor_Data;
with Sancta.Gui.Visor_Widget;
with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Network.Layer;
with Sancta.Network.Messages;

with Top_Visor_Pkg; use Top_Visor_Pkg;

with Agpl.Chronos;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gdk.Event; use Gdk.Event;
with Gtk.Menu;  use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;

private with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

package Sancta.Gui.Visor is

   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.gui.visor";

   type Object (Link : not null access Network.Layer.Object'Class)
     is new Netlistener.Object with private;
   type Object_Access is access all Object'Class;

   type Widget_Container is record
      Window : Gtk_Window;
      Area   : Gtk_Widget;
      Widget : Visor_Widget.Object_Access;
   end record;

   procedure Clear_Tasks (This : in Object);
   --  Send a Clear_Tasks to all people around

   procedure Create (This : in out Object;
                     Opts :        Config.Object);
   --  Create top window and subscribe to GUI messages and etc.

   procedure Create_Simple (This : in out Object;
                            Opts :        Config.Object);
   --  Create a simple view with only the mission and general views.

   procedure Create_Bare (This : in out Object;
                          Opts : Config.Object);
   --  Create the needed data structures, but no top window nor views.
   --  Useful to then get standalone view widgets with Create_View

   procedure Create_View (This : in out Object; Kind : in String);
   --  Create an instance of a registered view type.

   function Create_Widget (This : Object; Window : Gtk_Window; Kind : String)
                           return Widget_Container;

   procedure Emergency_Stop (This : in Object);
   --  Send an emergency stop to all people around.

   function Get_Data (This : in Object) return Visor_Data.Object_Access;
   --  Internal status access

   function Id (This : Object) return Node_Id;

   procedure Run (This : in out Object);
   --  Should be called frequently.

   function Is_Done (This : in Object) return Boolean;
   --  True when closing must happen.

   procedure Set_Done (This : in out Object);
   --  Instruct the visor to be done.

   --  CALLBACKS

   function Delete (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    Visor  :        Object_Access)
                    return          Boolean;

   procedure Clear_Clicked (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose;
                            Visor  :        Object_Access);
   --  Broadcast a Clear_Tasks

   procedure Stop_Clicked (Widget : access Gtk_Widget_Record'Class;
                           Event  :        Gdk.Event.Gdk_Event_Expose;
                           Visor  :        Object_Access);
   --  Emergency stop clicked!


   procedure View_Clicked (Widget : access Gtk_Widget_Record'Class;
                           Visor  :        Object_Access);
   --  Clicked on a menu item to get a new view.

private

   package Widget_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Widget_Container);

   use Ada.Calendar;

   type Object (Link : not null access Network.Layer.Object'Class) is new Netlistener.Object (Link) with
      record
         Self     : Object_Access;

         --           Config   : Sancta.Config.Object;
         Config   : Sancta.Gui.Config.Object;

         Shutdown : Boolean := False; -- When to exit...

         Data     : aliased Visor_Data.Object;
         Widgets  : Widget_Lists.List;

         Top       : Top_Visor_Pkg.Top_Visor_Access; -- Top window with alarm button...
         View_Menu : Gtk_Menu; -- View menu in top window.

         Laser_Scans : Natural := 4; -- Scans to keep and display per robot.
         Laser_Cron  : Agpl.Chronos.Object; -- To dissipate old laserscans

         Update_Cron : Agpl.Chronos.Object; -- For timed updates

         Recording_Next  : Ada.Calendar.Time := Ada.Calendar.Clock + 2.0;
         Recording_Index : Positive := 1;
      end record;

   procedure Create_Common (This : in out Object;
                            Opts :        Config.Object);

   procedure Create_Menu_Ver (This : in out Object;
                              Menu :        Gtk_Menu_Item);
   --  Attach the creators for view to the given Ver menu

   type Gtk_Menu_Item_With_View_Record is new Gtk_Menu_Item_Record with record
      Name : Ustring;
   end record;
   type Gtk_Menu_Item_With_View is access all Gtk_Menu_Item_With_View_Record'Class;
   --  We'll use this extension to keep in each menu item the name of the
   --  creator associated with it.

   procedure Local_Log (This    : in out Object;
                        Text    : in     String;
                        Level   : in     Agpl.Trace.Levels;
                        Section : in     String;
                        Sender  : in     String);

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Robot_Data.Network_Update);
   --  Process an incoming gui msg.

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Network.Messages.Laser_Type);
   --  Incoming laser reading

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Network.Messages.Redirect_Type);
   --  Messages from the plugin system

   overriding
   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);
   --  Overriding netlistener to process the packets of our interest

   procedure Update_All (This : in out Object;
                         Src  : in     Node_Id;
                         Msg  : in     Robot_Data.Network_Update);
   --  Propagate a gui update to all widgets.

   function Is_Recording (This : Object) return Boolean;

   procedure Capture_Views (This : in out Object);
   --  Save snapshots of all open views

end Sancta.Gui.Visor;
