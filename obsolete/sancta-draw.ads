with Sancta.Types;

with Sancta.Assignment;
with Sancta.Cost_Cache;
with Agpl.Gdk.Drawer;
with Agpl.Gdk.Palette;
with Agpl; use Agpl;

with Gtk.Widget; use Gtk.Widget;

package Sancta.Draw is

   --  Subprograms for drawing (non-modally) several EXPRES data

--     procedure Draw_Assignment
--       (This       : in Sancta.Assignment.Object;
--        Costs      : in Sancta.Cost_Cache.Object'Class := Sancta.Cost_Cache.Empty_Object;
--        Show_Costs : in Boolean := True);
   --  Draw the tasks pertaining to each robot.
   --  Uses Agpl.Gdk.Managed facilities, that you should start/stop accordingly.
   --  If Costs are supplied, they're used. If not, the agents in the assignment
   --  are probed
   --  If Show, text appears with each robot cost

   procedure Draw_Assignment
     (Ass        : in     Sancta.Assignment.Object;
      D          : in out Agpl.Gdk.Drawer.Object;
      Pal        : not null access Agpl.Gdk.Palette.Object;
      Widget     : in     Gtk_Widget;
      Costs      : in     Sancta.Cost_Cache.Object'Class := Sancta.Cost_Cache.Empty_Object;
      Show_Costs : in     Boolean := True);
   --  Draw in a given drawer object.
   --  We assume Pal, D have the proper drawable assigned.
   --  No D.Draw_Begin or D.Draw_End will be performed.
   --  Background will not be cleared.
   --  The widget must be valid only if Texts (Costs) are to be shown.

   procedure Draw_Laser (This      : in Types.Posed_Range_Scan_Vectors.Vector;
                         Max_Range : in Types.Real);
   --  Modal

end Sancta.Draw;
