with Sancta.Component.Root;

use Sancta;

package Sancta.Component.Manual_Tasks is

   --  Create given tasks at certain poses.

   --  Tasks are given in child elements:
   --  <task x="" y="" enabled="" />

   Log_Section          : constant String := "sancta.component.manual_tasks";

   Name                 : aliased constant Component_Name := "manual_tasks";

   Provides_Tasks       : constant Internal_Key := "tasks";

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   procedure Register;

private

   type Object is new Root.Object with null record;
   type Object_Access is access all Object;

end Sancta.Component.Manual_Tasks;
