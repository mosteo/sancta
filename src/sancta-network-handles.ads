with Agpl.Generic_Handle;

package Sancta.Network.Handles is

   pragma Preelaborate;

   subtype Message_Handle is Network.Message_Handle;

   package Address_Handles is new Agpl.Generic_Handle (Address);
   type Address_Handle is new Address_Handles.Object with null record;

   package Metadata_Handles is new Agpl.Generic_Handle (Message_Metadata);
   type Metadata_Handle is new Metadata_Handles.Object with null record;

end Sancta.Network.Handles;
