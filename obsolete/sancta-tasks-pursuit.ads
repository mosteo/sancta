--  This task attempts to fully sweep some location, without allowing any
--  evader to scape without being seen.

--  This first version of the algorithm is simplistic: once a robot is placed
--  at its "hold position" pose, it sents fellow robots to explore any gap
--  detected. After this, itself becomes "free" to be sent to other gaps.

with Sancta.Tasks.Types;
with Sancta.Types; use Sancta.Types;

with Sancta.Tasks.Compound;
with Agpl; use Agpl;

with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget;       use Gtk.Widget;

package Sancta.Tasks.Pursuit is

   pragma Elaborate_Body;

   type Object is new Sancta.Tasks.Compound.Object with private;

   function Create
     (Start : in Sancta.Types.Pose;
      Wait  : in Boolean := False)
      return Object;
   --  Start : Starting pose for the first robot entering the maze.
   --  Wait  : If the planning will be stopped waiting for user click.

   function Get_Data (This : in Object) return Types.Smart_Pursuit_Info.Object;

   function Get_Start (This : in Object) return Pose;

   procedure Get_Widget (This : in out Object; Widget : out Gtk_Widget);
   --  Get the widget for this task.

   procedure Redraw (This : in out Object);
   --  Force GUI update.

   procedure Set_Working_Area
     (This : in out Object;
      P1,
      P2   : in     Pose);
   --  Bounding box for the area to draw in the GUI.
   --  The widget must have been created.

   function To_String (This : in Object) return String;

private

   type Object is new Sancta.Tasks.Compound.Object with
      record
         Start : Pose;
         --  Starting pose from where the first robot will enter.
         --  Everything behind it is considered "clean".

         Must_Wait : Boolean := True;
         --  When true, at each algorithm critical step, the user is required
         --  to allow the continuation.

         Data      : Tasks.Types.Smart_Pursuit_Info.Object;
         --  Data that must be accesible for the GUI and several tasks.

         Area      : Gtk_Drawing_Area;

         Inited    : Boolean := False;
      end record;


end Sancta.Tasks.Pursuit;
