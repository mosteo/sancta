with Agpl.Strings;
use Agpl.Strings;

with Ada.Containers.Indefinite_Ordered_Maps;

package body Sancta.Ctree.Factory is

   package String_Object_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Object_Access);

   Store : String_Object_Maps.Map;

   --------------
   -- Register --
   --------------

   procedure Register
     (Id   : String;
      This : not null Object_Access)
   is
   begin
      Store.Insert (To_Lower (Id), This);
   end Register;

   -------------
   -- Recover --
   -------------

   function Recover (Id : String) return not null Object_Access is
   begin
      if Store.Contains (To_Lower (Id)) then
         return Store.Element (To_Lower (Id));
      else
         raise Constraint_Error with "Object is not registered: " & Id;
      end if;
   end Recover;

end Sancta.Ctree.Factory;
