with Sancta.Gui.Robot_Data;
with Sancta.Gui.Visor_Data;
with Sancta.Gui.Visor_Widget;

with Agpl.Chronos;
with Agpl.Gdk.Drawer;
with Agpl.Gdk.Palette;

with Gdk.Event;        use Gdk.Event;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;

--  Visual database view

package Sancta.Gui.Visor_Dbv_View is

   Log_Section    : constant String := "sancta.gui.visor_dbv_view";
   Detail_Section : constant String := "sancta.gui.visor_dbv_view.detail";

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

   type Object is new Visor_Widget.Object with
      record
         Data         : Visor_Data.Object_Access;

         Area         : Gtk_Drawing_Area; -- Where to paint.

         --  Art
         Pal          : aliased Agpl.Gdk.Palette.Object;
         Drawer       : Agpl.Gdk.Drawer.Object;

         Redraw_Timer : Agpl.Chronos.Object;
      end record;

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    This   :        Visor_Widget.Object_Access)
                    return          Boolean;
   --  Callback for "expose-event".

   procedure Redraw (This : in out Object);
   --  Use this instead Widget.Queue_Draw, to throttle drawing

end Sancta.Gui.Visor_Dbv_View;
