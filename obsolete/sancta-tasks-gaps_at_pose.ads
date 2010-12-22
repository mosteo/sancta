with Sancta.Tasks.Positioned;
with Sancta.Tasks.Types;
with Sancta.Types;

with Sancta.Gap.Vector;

package Sancta.Tasks.Gaps_At_Pose is

   pragma Elaborate_Body;

   --  Task description:
   --  Go to the given pose if not there.
   --  Store the gaps seen from there.

   type Object is new Tasks.Positioned.Object with
      record
         Gaps       : Sancta.Gap.Vector.Object (First => 1);
         --  Gaps found at pose.

         Pursuit_Data : Types.Smart_Pursuit_Info.Object;
         --  Reference to the global Pursuit data.
      end record;

   function Create
     (Data : in Types.Smart_Pursuit_Info.Object;
      Pose : in Sancta.Types.Pose) return Object;
   --  Create a task of this type.

   function To_String (This : in Object) return String;

end Sancta.Tasks.Gaps_At_Pose;
