with Sancta.Debug;

package body Sancta.Tasks.Cover_Gaps_And_Dispatch is

   ------------
   -- Create --
   ------------

   function Create (Data         : in Types.Smart_Cover.Object;
                    Pursuit_Data : in Types.Smart_Pursuit_Info.Object;
                    Gaps         : in Sancta.Gap.Vector.Object;
                    Pose         : in Sancta.Types.Pose
                   ) return Object
   is
      This : Object;
   begin
      This.Data         := Data;
      This.Pursuit_Data := Pursuit_Data;
      This.Gaps         := Gaps;
      This.Pose         := Pose;

      return This;
   end Create;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (This : in Object) return Types.Smart_Cover.Object is
   begin
      return This.Data;
   end Get_Data;

   --------------
   -- Get_Gaps --
   --------------

   function Get_Gaps (This : in Object) return Sancta.Gap.Vector.Object is
   begin
      return This.Gaps;
   end Get_Gaps;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : in Object) return Sancta.Types.Pose is
   begin
      return This.Pose;
   end Get_Pose;

   ----------------------
   -- Get_Pursuit_Data --
   ----------------------

   function Get_Pursuit_Data (This : in Object) return Types.Smart_Pursuit_Info.Object is
   begin
      return This.Pursuit_Data;
   end Get_Pursuit_Data;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return "Cover gaps at " & Debug.To_String (This.Pose) & " and dispatch";
   end To_String;

end Sancta.Tasks.Cover_Gaps_And_Dispatch;
