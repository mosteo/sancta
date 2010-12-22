--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Root;

package Sancta.Component.Spawn is

   Log_Section : constant String := "sancta.component.spawn";

   Name : aliased constant Component_Name := "spawn";

   Option_Command : constant Option_Attr  := "command";

   Option_Info    : constant Option_Attr  := "info";
   Default_Info   : constant Option_Attr  := "";
   --  Some log message to be shown after successful spawning

   Option_Wait    : constant Option_Attr  := "wait"; -- boolean
   Default_Wait   : constant Boolean      := False;
   --  Wait for command completion or not.
   --  If yes, will block at Create until done

   procedure Register;

private

   type Object is new Root.Object with null record;

   function Create (Config : in Comp_Config)
                    return      Component.Object_Access;

end Sancta.Component.Spawn;
