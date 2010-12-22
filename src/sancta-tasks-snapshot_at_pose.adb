with Sancta.Debug;
with Sancta.Types;

with Sancta.Tasks;

package body Sancta.Tasks.Snapshot_At_Pose is

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return
        "Snap at" &
      Debug.To_String (This.Pose.X) & "," &
      Debug.To_String (This.Pose.Y) & "," &
      Debug.To_String (Types.Real (This.Pose.A)) & " (#" &
      Sancta.Tasks.Task_Id'Image (This.Get_Id) & ")";
   end To_String;

end Sancta.Tasks.Snapshot_At_Pose;
