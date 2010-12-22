with Sancta.Gui.Robot_Data;
with Sancta.Gui.Visor_Data;
with Sancta.Gui.Visor_Widget;
with Sancta.Network;

with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Tree_View;       use Gtk.Tree_View;
with Gtk.Widget;          use Gtk.Widget;

--  Just for showing log messages being generated in other robots

package Sancta.Gui.Visor_Log_View is

   type Object (<>) is new Visor_Widget.Object with private;
   type Object_Access is access all Object'Class;

   function Get_Widget (This : in Object) return Gtk_Widget;
   --  The visible part.

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object);
   --  Pass the data pointer to the object.

   procedure Update (This : in out Object;
                     Id   : in     Node_Id; -- Originator of msg.
                     Msg  : in     Robot_Data.Network_Update);

   procedure Register;

private

   type Object is new Visor_Widget.Object with
      record
         Data     : Visor_Data.Object_Access;

         Scroll   : Gtk_Scrolled_Window;
         Log_Tree : Gtk_Tree_View;
      end record;

end Sancta.Gui.Visor_Log_View;
