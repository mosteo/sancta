with Player; use Player;
with Player.Debug; use Player.Debug;
with Player.Math;  use Player.Math;

with Text_Io; use Text_Io;

procedure T002_Robot_To_World is
   Robot_Pose     : constant Pose := (0.0, 0.0, 0.0);
   Point_In_Robot : constant Pose := (0.0, 1.0, 0.0);
   Expected_Pose  : constant Pose := (0.0, 1.0, 0.0);
begin
   if Robot_To_World (Point_In_Robot, Robot_Pose) /= Expected_Pose then
      Put_Line ("FAIL.");
      Put_Line ("Got " & To_String (Robot_To_World (Point_In_Robot, Robot_Pose)));
      Put_Line ("Exp " & To_String (Expected_Pose));
   else
      Put_Line ("PASS.");
   end if;
end T002_Robot_To_World;
