with Ada.Finalization;

package Sancta.Ctree.Factory is

   pragma Elaborate_Body;

   type Object is new Ada.Finalization.Limited_Controlled with null record;

   type Object_Access is access all Object'Class;

   procedure Register (Id   : String;
                       This : not null Object_Access);

   function Recover (Id : String) return not null Object_Access;

end Sancta.Ctree.Factory;
