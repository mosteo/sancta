with Agpl.Random;

function Sancta.Tasks.Entry_Point.Flip (This : in Object) return Object is
   Result : Object := This;
   use Types.Pose_Vector;
begin
   Set_Pose (Result,
     This.Candidates.Vector
       (Agpl.Random.Get_Integer (This.Candidates.First,
        Last (This.Candidates))));
   return Result;
end Sancta.Tasks.Entry_Point.Flip;
