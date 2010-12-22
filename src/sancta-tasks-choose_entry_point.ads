with Sancta.Types;

with Sancta.Tasks.Compound;

--  This task differs from Goto_Pose in that the pose is reached but the
--  task is not finished immediately. It is used as child of Pursuit tasks.

package Sancta.Tasks.Choose_Entry_Point is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Compound.Object with private;

   function Candidates (This : in Object) return Sancta.Types.Pose_Array;

   function Create (Candidates : in Sancta.Types.Pose_Array)
                    return Object;
   --  This task is doable just as a first task. It implies that the agent
   --  should be placed in one of those starting poses.

   function To_String (This : Object) return String;

private

   type Object is new Sancta.Tasks.Compound.Object with
      record
         Candidates : Sancta.Types.Pose_Vector.Object (First => 1);
      end record;

end Sancta.Tasks.Choose_Entry_Point;
