with Sancta.Gui.User_Data_Handle;
with Sancta.Tasks.Wander;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;

with Ada.Calendar; use Ada;

package Sancta.Tasks.Wander_For_Gaps is

   pragma Elaborate_Body;

   type Object;
   type Object_Access is access all Object;

   type Object is new Sancta.Tasks.Wander.Object with record
      Last_Peek : Calendar.Time := Calendar.Clock;
      Reload    : Duration      := 1.0; -- Time between peeks
      Area      : Gtk_Drawing_Area;

      Gui_Data  : Sancta.Gui.User_Data_Handle.Object;

      Inited    : Boolean := False;
   end record;
   --  Used just for debugging the Extract_Gaps function.

   procedure Create_Widget (This   : in out Object;
                            Widget :    out Gtk_Widget);
   --  Create and return the widget for this task.

   procedure Redraw (This : in out Object);

end Sancta.Tasks.Wander_For_Gaps;
