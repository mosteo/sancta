with Sancta.Datastore;
with Sancta.Component;

package body Sancta.Component.Hardwired is

   --------------
   -- Get_Link --
   --------------

   function Get_Link return Network.Layer.Object_Access is
   begin
      return
        Network.Layer.Object_Access
          (Datastore.Network_Layer
               (Datastore.Object.Get (Component.Link_Key)).Ref);
   end Get_Link;

end Sancta.Component.Hardwired;
