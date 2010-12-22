with Sancta.Gui.Robot_Data;
with Sancta.Gui.Visor_Data,
     Sancta.Gui.Visor_Factory;
with Sancta.Gui.Visor_Widget;
with Sancta.Network;
with Sancta.Types;

with Agpl.Chronos;
with Agpl.Gdk.Drawer;
with Agpl.Gdk.Palette;
with Agpl.Timed_Transformer;
with Sancta.Plan;
with Sancta.Tasks;

with Gdk.Event;        use Gdk.Event;
--  with Glade.Xml;        use Glade.Xml;
with Glib;             use Glib;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;

--  This is a general top-projection view of robots and tasks.

package Sancta.Gui.Visor_General_View is

   Log_Section    : constant String := "sancta.gui.visor_general_view";
   Detail_Section : constant String := "sancta.gui.visor_general_view.detail";

   View_Name : constant String := "General view";

   Minimum_Redraw_Wait : constant Duration := 0.05;
   --  Minimum amount of time between two drawings, not to hog the CPU

   type Object (<>) is new Visor_Widget.Object with private;
   type Object_Access is access all Object'Class;

   function Get_Widget (This : in Object) return Gtk_Widget;
   --  The visible part.

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object);

   procedure Update (This : in out Object;
                     Id   : in     Node_Id; -- Originator of msg.
                     Msg  : in     Robot_Data.Network_Update);
   --  Update received; must be drawn.

   procedure Register;

private

   package Float_Tt is new Agpl.Timed_Transformer (Float);

   type Drag_Kinds is (Xy, Ang, Invalid);

   type Laser_Draw_Kinds is (Points, Segments);

   type Interaction_Modes is (Drag_Robots, Go_To_Place, Guide_Robot,
                              Auction_Goal);

   type Zoom_Modes is (Auto, Fixed, Follow, Mean);

   type Object is new Visor_Widget.Object with
      record
         --  Configuration of user interaction:
         Interaction  : Interaction_Modes := Drag_Robots;

         Former_Bot   : Visor_Data.Robot_Access; -- Previously selected bot.
         Selected_Bot : Visor_Data.Robot_Access; -- The one being commanded.

         Data         : Visor_Data.Object_Access;

         Area : Gtk_Drawing_Area; -- Where to paint.

         --  Art
         Pal         : aliased Agpl.Gdk.Palette.Object;
         Drawer      : Agpl.Gdk.Drawer.Object;
         --  We must keep it ready to have the transformation during Drag'n'Drop
         Drawer_Zoom  : Float_Tt.Object := Float_Tt.Create (10.0);
         Drawer_Zoom_Mode : Zoom_Modes := Mean;
         Drawer_Zoom_X    : Float      := 0.0;
         Drawer_Zoom_Y    : Float      := 0.0;

         Draw_Sight : Boolean := False;
         Draw_Grid  : Boolean := False;
         Draw_Pose  : Boolean := False;

         Dnd_Ready : Boolean := False;
         --  We will prepare Drag'n'Drop in the first expose event

         Dragging  : Boolean := False;
         Drag_Kind : Drag_Kinds; -- Dragging pose or angle?
         Drag_Pose : Types.Pose; -- Last dragged values in Pose coordinates
         Drag_X    : Gint; -- Initial screen X at drag start (for dragging angle)
         Drag_A    : Types.Angle; -- Initial angle at drag start

--         Dialog_Set_Pose : Glade_Xml;

         Laser_Draw  : Laser_Draw_Kinds := Segments;

         Redraw_Timer     : Agpl.Chronos.Object;

         Out_Of_Range : Types.Real := 32.0; -- For laser

         --  Keyboard section
         Caps_On      : Boolean := False;
      end record;

   procedure Adjust_Zoom (This : in out Object);
   --  Re-center the zoom ranges around the selected robot, or auto if no robot

   procedure Correct_Angle (This : in out Object;
                            Bot  : in     Visor_Data.Robot_Access);
   --  Try to align the given robot with the environment

   function Create (Context : Visor_Factory.Creation_Context)
                    return    Visor_Widget.Object_Access;
   --  Creates a new widget of this kind.

   function Clicked (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk.Event.Gdk_Event_Button;
                     This   :        Visor_Widget.Object_Access)
                     return   Boolean;
   --  Callback for "button_press_event"

   procedure Dialog_Set_Criterion (This : in out Object);

   procedure Dialog_Set_Pose (This : in out Object);
   --  Show a dialog for fine tuning of the belief pose.

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    This   :        Visor_Widget.Object_Access)
                    return          Boolean;
   --  Callback for "expose-event".

   function Key_Press (Widget : access Gtk_Widget_Record'Class;
                       Event  :        Gdk.Event.Gdk_Event_Key;
                       This   :        Visor_Widget.Object_Access)
                       return Boolean;
   function Key_Release (Widget : access Gtk_Widget_Record'Class;
                       Event  :        Gdk.Event.Gdk_Event_Key;
                       This   :        Visor_Widget.Object_Access)
                       return Boolean;
   --  Callback for keyboard handling

   procedure Match_Two_Bots (This       : in Object;
                             Bot1, Bot2 : in Visor_Data.Robot_Access);
   --  Do matching between two robot laserscans and correct the pose of the 2nd.

   procedure Mission_Acquire (This : in out Object);
   --  Use the current robot tasks as initial mission.

   procedure Mission_Start (This : in out Object);
   --  Do the mission launching. This should be moved from here to the visor
   --  mission window, but no time...

   procedure Mission_Start (This : in out Object;
                            Plan : in     Sancta.Plan.Object);

   procedure Prepare_Dnd (This : in out Object);
   --  Called in the first expose, to prepare drag'n'drop

   procedure Prepend_Task (This : in out Object;
                           Bot  : in     Visor_Data.Robot_Access;
                           Job  : in     Sancta.Tasks.Object'Class);

   procedure Print_Help;

   procedure Redraw (This : in out Object);
   --  Use this instead Widget.Queue_Draw, to throttle drawing

   procedure Remove_First_Task (This : in out Object;
                                Bot  : in     Visor_Data.Robot_Access);

   procedure Robot_Drag_End
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access);

   function Robot_Drag_Happening
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access) return Boolean;

   procedure Send_Message (This : in out Object;
                           Bot  : in     Visor_Data.Robot_Access;
                           Msg  : in     Network.Message'Class);

   procedure Set_Criterion_Ok
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access);

   procedure Set_Pose_Ok
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access);

end Sancta.Gui.Visor_General_View;
