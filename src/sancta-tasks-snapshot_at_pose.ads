with Sancta.Tasks.Positioned;

package Sancta.Tasks.Snapshot_At_Pose is

   pragma Preelaborate;

   type Object is new Tasks.Positioned.Object with null record;

   function To_String (This : in Object) return String;

end Sancta.Tasks.Snapshot_At_Pose;
