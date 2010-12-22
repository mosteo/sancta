with Sancta.Debug;

package body Sancta.Tasks.Gaps_At_Pose is

   ------------
   -- Create --
   ------------

   function Create
     (Data : in Types.Smart_Pursuit_Info.Object;
      Pose : in Sancta.Types.Pose) return Object
   is
      This : Object;
   begin
      This.Pose         := Pose;
      This.Pursuit_Data := Data;
      return This;
   end Create;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return "Gaps seen from " & Debug.To_String (This.Pose);
   end To_String;

end Sancta.Tasks.Gaps_At_Pose;
