with Sancta.Gui.Robot_Data;
with Sancta.Gui.Visor_Data;
with Sancta.Network;

with Gtk.Widget; use Gtk.Widget;

with Ada.Unchecked_Deallocation;

--  Visor widgets are things that show information and which its final expression
--  is a Gtk_Widget. This could allow for future dockability or something.
--  In the meantime, is an abstract interface for update notification

--  Each widget must take care of its own signalling.

package Sancta.Gui.Visor_Widget is

   --  pragma Elaborate_Body;

   type Object is abstract tagged limited null record;
   type Object_Access is access all Object'Class;

   function Get_Widget (This : in Object) return Gtk_Widget is abstract;
   --  The visible part.

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object) is abstract;
   --  Pass the data pointer to the object.

   procedure Update (This : in out Object;
                     Id   : in     Node_Id; -- Originator of msg.
                     Msg  : in     Robot_Data.Network_Update) is abstract;
   --  This is called outside of "expose" events in response of a message.
   --  So the widget must keep the data it needs and invalidate itself.

   procedure Free is
     new Ada.Unchecked_Deallocation (Object'Class, Object_Access);

end Sancta.Gui.Visor_Widget;
