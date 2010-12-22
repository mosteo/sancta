with Sancta.Tasks.Types;
with Sancta.Types;

with Sancta.Gap.Vector;
with Sancta.Tasks.Compound;
with Agpl; use Agpl;

package Sancta.Tasks.Cover_Gaps_And_Dispatch is

   pragma Elaborate_Body;

   --  Task description:
   --  The current gaps in the task must be maintained under sight always.
   --  Agents must be dispatched to cover these gaps from new positions.
   --  Task finishes when the gaps are covered from another improved position.

   type Object is new Sancta.Tasks.Compound.Object with private;

   function Create (Data         : in Types.Smart_Cover.Object;
                    Pursuit_Data : in Types.Smart_Pursuit_Info.Object;
                    Gaps         : in Sancta.Gap.Vector.Object;
                    --  Gaps being seen
                    Pose         : in Sancta.Types.Pose
                    --  Pose from where the observation is taking place
                   ) return Object;

   function Get_Data (This : in Object) return Types.Smart_Cover.Object;

   function Get_Gaps (This : in Object) return Sancta.Gap.Vector.Object;

   function Get_Pose (This : in Object) return Sancta.Types.Pose;

   function Get_Pursuit_Data (This : in Object) return Types.Smart_Pursuit_Info.Object;

   function To_String (This : in Object) return String;

private

   type Object is new Sancta.Tasks.Compound.Object with
      record
         Data         : Tasks.Types.Smart_Cover.Object;
         Pursuit_Data : Tasks.Types.Smart_Pursuit_Info.Object;

         Gaps  : Sancta.Gap.Vector.Object (First => 1);
         --  Gaps to cover.

         Pose  : Sancta.Types.Pose;
         --  Position from where they were previously observed.

         Goals : Sancta.Types.Pose_Vector.Object (First => 1);
         --  Goals to reach.
      end record;

end Sancta.Tasks.Cover_Gaps_And_Dispatch;
