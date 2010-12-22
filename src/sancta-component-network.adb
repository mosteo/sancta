with Ada.Unchecked_Deallocation,
     Sancta.Component.Factory,
     Sancta.Network.Layer.Root;
with Sancta.Network.Layer.Tcp;
with Sancta.Network.Layer.Udp;

package body Sancta.Component.Network is

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
      case Kinds'Value (This.Option (Option_Kind, "")) is
         when Tcp =>
            This.Link := new Sancta.Network.Layer.Tcp.Object;
         when Udp =>
            This.Link := new Sancta.Network.Layer.Udp.Object;
         when Rtwmp =>
            raise Program_Error with "Moved to network_rtwmp component";
         when others =>
            raise Program_Error with "Network unimplemented: " &
              This.Option (Option_Kind);
      end case;

      declare
         Net  : Sancta.Network.Layer.Root.Object'Class renames
           Sancta.Network.Layer.Root.Object'Class (This.Link.all);
      begin
         Net.Set_Id (Env.Id);
         Net.Initialize (Config);
      end;

      This.Output (Provides_Link, Network'(Link => This.Link));

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

   ----------
   -- Link --
   ----------

   function Link (This : Object) return Sancta.Network.Layer.Object_Access is
   begin
      return This.Link;
   end Link;

end Sancta.Component.Network;
