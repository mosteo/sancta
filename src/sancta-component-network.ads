--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Ctypes;
with Sancta.Component.Environment,
     Sancta.Component.Root,
     Sancta.Network.Layer;

package Sancta.Component.Network is

   Log_Section : constant String := "sancta.component.network";

   --       <component name="network" kind="udp">
   --           <provides data="link" as="link" />
   --
   --           <node id="Ari" address="192.168.1.1:7777"/>
   --           <node id="Ben" address="192.168.1.2:7777"/>
   --           <node id="Ced" address="192.168.1.3:7777"/>
   --           <node id="Dan" address="192.168.1.4:7777"/>
   --
   --           <!--  Groups are used for multicast  -->
   --           <group id="auctions">
   --              <member id="Ari"/>
   --              <member id="Ben"/>
   --              <member id="Ced"/>
   --              <member id="Dan"/>
   --           </group>
   --        </component>


   Name : aliased constant Component_Name := "network";

   Opt_Port      : constant Option_Attr := "port";
   Opt_Nodes     : constant Option_Attr := "nodes";
   --  If present, it is a dot separated list of enabled nodes. Only these
   --    will be configured.
   --  If absent, ALL nodes listed will be configured.

   Provides_Link : constant Internal_Key := "link";

   Option_Kind : constant Option_Attr := "kind";
   type Kinds is (Tcp, Udp, Udp_Split, Rtwmp);

   type Object is new Root.Object with private;

   function Link (This : Object) return Sancta.Network.Layer.Object_Access;

   procedure Register;

   --  for convenience
   subtype Network is Ctypes.Network;

private

   type Object is new Root.Object with record
      Link : Sancta.Network.Layer.Object_Access;
   end record;

   function Create (Config : in Agpl.Xml.Node;
                    Env    : in Environment.Object)
                    return      Component.Object_Access;

   overriding
   procedure Stop (This : in out Object);

end Sancta.Component.Network;
