with Sancta.Ctree.Ad_Hoc_Cost_Generator,
     Sancta.Component.Factory,
     Sancta.Component.Helper,
     Sancta.Component.Types;

package body Sancta.Ctree.Component.Ad_Hoc_Cost_Generator is

   type Object_Access is access all Object;

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
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Agent_Proxy.Object_Access
                    (Types.Agent (Help.Input (Requires_Agent)).Agent));
   begin
      This.Agent.Set_Cost_Generator
        (Sancta.Ctree.Ad_Hoc_Cost_Generator.Create (This.Agent.Get_Pose));

      return Component.Object_Access (This);
   end Create;

end Sancta.Ctree.Component.Ad_Hoc_Cost_Generator;
