with Sancta.Component.Factory;

package body Sancta.Component.Skeleton is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
   begin
      return new Object (Name'Access, Config);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Skeleton;
