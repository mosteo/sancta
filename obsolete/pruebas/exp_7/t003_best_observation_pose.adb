with Sancta.Debug; use Sancta.Debug;
with Sancta.Types; use Sancta.Types;
with Sancta.Types.Operations; use Sancta.Types.Operations;

with Agpl.Constants; use Agpl.Constants;

with Text_Io; use Text_Io;

procedure T003_Best_Observation_Pose is
begin
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((0.0, 0.0, 0.0),
                           (1.0, 0.0, 0.0), Pi_2)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((0.0, 0.0, 0.0),
                           (2.0, 0.0, 0.0), Pi_2)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((1.0, 1.0, 0.0),
                           (2.0, 2.0, 0.0), Pi_2)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((1.0, 1.0, 0.0),
                           (2.0, 0.0, 0.0), Pi_2)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((1.0, 1.0, 0.0),
                           (2.0, 0.0, 0.0), Pi_4)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((1.0, 1.0, 0.0),
                           (2.0, 0.0, 0.0), Pi)));
   Put_Line ("Obs: " &
             To_String (Best_Observation_Pose
                          ((1.0, 1.0, 0.0),
                           (2.0, 0.0, 0.0), Three_Pi_2)));
end T003_Best_Observation_Pose;
