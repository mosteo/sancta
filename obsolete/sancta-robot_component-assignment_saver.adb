with Sancta.Plugin.Factory;
pragma Elaborate_All (Sancta.Plugin.Factory);

package body Sancta.Plugin.Skeleton is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Plugin.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      return Plugin.Object_Access (This);
   end Create;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Plugin.Skeleton;
