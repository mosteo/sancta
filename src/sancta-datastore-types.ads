with Sancta.Distributed.Datastore;
with Sancta.Robot_Actions;

package Sancta.Datastore.Types is

--   pragma Preelaborate;

   type Shared_Database (Ref : access Distributed.Datastore.Object'Class) is new
     Agpl.Protected_Datastore.Object_Data with null record;

   type Robot_Action is new Agpl.Protected_Datastore.Object_Data with record
      Robot_Action : Robot_Actions.Object;
   end record;

end Sancta.Datastore.Types;
