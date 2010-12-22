with Sancta.Tasks.Positioned;
with Sancta.Types;

with Sancta.Tasks.Immediate;

--  This task differs from Goto_Pose in that the pose is reached but the
--  task is not finished immediately. It is used as child of Pursuit tasks.

package Sancta.Tasks.Entry_Point is

   pragma Preelaborate;

   type Object is new Tasks.Positioned.Object
                  and Sancta.Tasks.Immediate.Object with private;

   function Create (Pose       : in Sancta.Types.Pose;
                    Candidates : in Sancta.Types.Pose_Array)
                    return Object;
   --  This task is doable just as a first task.
   --  It means that the agent should be introduced in the scene at Pose.

   function Candidates (This : in Object) return Sancta.Types.Pose_Array;

   function To_String (This : Object) return String;

private

   type Object is new Tasks.Positioned.Object
                  and Sancta.Tasks.Immediate.Object with
      record
         Candidates : Sancta.Types.Pose_Vector.Object (First => 1);
      end record;

end Sancta.Tasks.Entry_Point;
