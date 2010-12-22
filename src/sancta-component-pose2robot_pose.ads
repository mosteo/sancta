with Sancta.Component.Cast;
with Sancta.Component.Types;

package Sancta.Component.Pose2Robot_Pose is

   function To_Robot_Pose (P : Types.Pose) return Types.Robot_Pose;
   pragma Inline (To_Robot_Pose);

   package Caster is new Sancta.Component.Cast
     ("pose2robot_pose",
      Types.Pose,
      Types.Robot_Pose,
      To_Robot_Pose);

end Sancta.Component.Pose2Robot_Pose;
