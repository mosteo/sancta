with Sancta.Component;

package Sancta.Events is

   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.events";

   procedure Start_Plugin (X : in Component.Object_Access);

   procedure Start_Events;
   --  Call it after all the plugins have been started!

   procedure Stop_Plugins;

end Sancta.Events;
