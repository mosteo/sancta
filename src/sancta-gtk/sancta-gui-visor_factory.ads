with Sancta.Gui.Config,
     Sancta.Gui.Visor_Widget;

with Ada.Containers.Indefinite_Ordered_Maps;

package Sancta.Gui.Visor_Factory is

   --   pragma Elaborate_Body;

   type Creation_Context is record
      Options : Sancta.Gui.Config.Object;
   end record;

   type Creator is access
     function (Context : Creation_Context) return Visor_Widget.Object_Access;
   --  This function must take care of all creation and signal bundling for
   --  the widget.

   procedure Register (This : in Creator;
                       Name : in String);
   --  Add a creator for widgets of the given name.
   --  Visors can use the registered creators to obtain new widgets.

   package Creator_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Creator);

   Creators : Creator_Maps.Map; -- The registered creators.

end Sancta.Gui.Visor_Factory;
