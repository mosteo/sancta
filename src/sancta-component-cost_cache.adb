with Ada.Unchecked_Deallocation,
     Sancta.Component.Factory,
     Sancta.Cost_Matrix;

package body Sancta.Component.Cost_Cache is

   type Object_Access is access all Object'Class;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access,
                                                   Config);
   begin
      This.Verify (Option_Kind);
      case Kinds'Value (This.Option (Option_Kind, "")) is
         when Matrix =>
            This.Ptr := new Cost_Matrix.Object;
      end case;

      This.Output (Provides_Costs, Cost_Cache'(Ptr => This.Ptr));

      return Component.Object_Access (This);
   end Create;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (This : in out Object)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sancta.Cost_Cache.Object'Class, Sancta.Cost_Cache.Object_Access);
   begin
      Free (This.Ptr);
   end Stop;

end Sancta.Component.Cost_Cache;
