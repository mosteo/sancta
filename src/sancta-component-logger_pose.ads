with Sancta.Component.Logger_Data;

with Agpl.Protected_Datastore;

pragma Elaborate_All (Sancta.Component.Logger_Data);

package Sancta.Component.Logger_Pose is

   pragma Elaborate_Body;

   function To_String (This : in Agpl.Protected_Datastore.Object_Data'Class)
                       return    String;

   package Plugin_Logger_Pose is new
     Component.Logger_Data ("logger_pose",
                               To_String);

end Sancta.Component.Logger_Pose;
