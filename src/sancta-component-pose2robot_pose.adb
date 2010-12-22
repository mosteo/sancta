package body Sancta.Component.Pose2Robot_Pose is

   -------------------
   -- To_Robot_Pose --
   -------------------

   function To_Robot_Pose (P : Types.Pose) return Types.Robot_Pose is
   begin
      return Types.Robot_Pose'(Pose => P.Pose);
   end To_Robot_Pose;

end Sancta.Component.Pose2Robot_Pose;
