with Sancta.Gui.Robot_Data,
     Sancta.Gui.Visor_Data,
     Sancta.Gui.Visor_Factory,
     Sancta.Gui.Visor_Widget;

with Gtk.Widget; use Gtk.Widget;

--  This view offers a button for every task in the mission, so they can be
--   launched selectively.

package Sancta.Gui.Visor_Mission is

   --  pragma Elaborate_Body;

   Log_Section : constant String := "sancta.gui.visor_mission";

   View_Name : constant String := "Mission view";

   type Object is new Visor_Widget.Object with private;

   overriding
   function Get_Widget (This : in Object) return Gtk_Widget;

   overriding
   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object);

   procedure Register;

private

   type Object_Access is access all Object;

   overriding
   procedure Update (This : in out Object;
                     Id   : in     Node_Id; -- Originator of msg.
                     Msg  : in     Robot_Data.Network_Update) is null;

   type Object is new Visor_Widget.Object with record
      Data   : access Visor_Data.Object;
      Widget :        Gtk_Widget;
      This   :        Object_Access := Object'Unchecked_Access;
   end record;

   function Create (Context : Visor_Factory.Creation_Context)
                    return    Visor_Widget.Object_Access;

   procedure Create_Buttons (This : in out Object);
   pragma Precondition (This.Data /= null);

end Sancta.Gui.Visor_Mission;
