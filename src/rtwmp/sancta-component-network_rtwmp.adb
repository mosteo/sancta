with Ada.Unchecked_Deallocation,
     Sancta.Component.Factory,
     Sancta.Component.Network,
     Sancta.Network.Layer.Root;
with Sancta.Network.Layer.Rtwmp_Split;

package body Sancta.Component.Network_Rtwmp is

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node;
                    Env    : in Environment.Object)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Verify (Opt_Nodes);

      This.Link := new Sancta.Network.Layer.Rtwmp_Split.Object;

      declare
         Net  : Sancta.Network.Layer.Root.Object'Class renames
           Sancta.Network.Layer.Root.Object'Class (This.Link.all);
      begin
         Net.Set_Id (Env.Id);
         Net.Initialize (Config);
      end;

      This.Output (Provides_Link, Network.Network'(Link => This.Link));

      return Component.Object_Access (This);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Sancta.Network.Layer.Object'Class,
         Sancta.Network.Layer.Object_Access);
   begin
      This.Link.Shutdown;

      Free (This.Link);
   end Stop;

end Sancta.Component.Network_Rtwmp;
