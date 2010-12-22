--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Environment,
     Sancta.Component.Root,
     Sancta.Network.Layer;

package Sancta.Component.Network_Rtwmp is

   Log_Section : constant String := "sancta.component.network_rtwmp";

   --       <component name="network_rtwmp" nodes="ari.ben.ced.zak">
   --           <provides data="link" as="link" />
   --
   --           <!--  Groups are used for multicast  -->
   --           <group id="auctions">
   --              <member id="Ari"/>
   --              <member id="Ben"/>
   --              <member id="Ced"/>
   --              <member id="Dan"/>
   --           </group>
   --        </component>


   Name : aliased constant Component_Name := "network_rtwmp";

   Provides_Link : constant Internal_Key := "link";

   Opt_Nodes : constant Option_Attr := "nodes";
   --  Required, dot separated enumeration of live nodes.
   --  Since addresses have to be consecutive, this way we can autoconf the net.

   procedure Register;

private

   type Object is new Root.Object with record
      Link : Sancta.Network.Layer.Object_Access;
   end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    : in Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Stop (This : in out Object);

end Sancta.Component.Network_Rtwmp;
