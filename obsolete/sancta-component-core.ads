with Sancta.Component.Root;

with Agpl.Xml;

--  This core component is the one in charge of communicating with the basic
--  robot library. This one is for player.

package Sancta.Component.Core is

   pragma Elaborate_Body;

   Plugin_Name            : constant String := "core";

   type Object is new Root.Object with private;
   --  We implement different robotic capabilities as plugins who manipulate
   --  a common protected datastore. So each component can run synchronously
   --  or have its own tasks.

   function Create (Config : in Agpl.Xml.Node) return Object_Access;
   --  For a factory approach

   overriding
   procedure Stop (This : in out Object);

private

   task type Poll_Task (Parent : access Object) is
      entry Start;
      entry Stop;
   end Poll_Task;

   type Object is new Root.Object with record
      Reader : Poll_Task (Object'Access);
   end record;

end Sancta.Component.Core;
