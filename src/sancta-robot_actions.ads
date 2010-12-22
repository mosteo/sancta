with Sancta.Types;

package Sancta.Robot_Actions is

   --  Possible commands for robots are defined here.
   --  A particular implementation must understand these (like sancta.component.player_executor).

   type Kinds is (Goto_Pose, Goto_Pose_No_Angle, Set_Velocity);

   type Object (Kind : Kinds := Goto_Pose) is record
      case Kind is
         when Goto_Pose | Goto_Pose_No_Angle =>
            World_Goal : Types.Pose;
         when Set_Velocity =>
            Velocity   : Types.Pose;
      end case;
   end record;

end Sancta.Robot_Actions;
