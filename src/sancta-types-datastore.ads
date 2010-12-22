with Agpl.Protected_Datastore;

package Sancta.Types.Datastore is

   pragma Preelaborate;

   type Pose is new Agpl.Protected_Datastore.Object_Data with record
      Pose : Types.Pose;
   end record;

end Sancta.Types.Datastore;
