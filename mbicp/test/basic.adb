with Mbicp; use Mbicp;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_Io; use Ada.Text_Io;

procedure Basic is
   L1, L2 : Laser_Scan (1 .. 180);
   Step   : constant Mbicp_Float := 3.0 / 180.0;
   A      : Mbicp_Float := -1.5;

   Odom   : constant Pose := (1.0, 0.0, 0.0);
   Result : Pose;
   Code   : Outcomes;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Mbicp_Float);
   use Math;
begin
   Set_Out_Of_Range (32.0);

   Init (Max_Iter => 1000);

   for I in L1'Range loop
      L1 (I).Distance := 30.0 / Cos (A); -- A wall at 30.0 units
      L2 (I).Distance := 28.0 / Cos (A); -- A wall at 29.0 units
      L1 (I).Bearing  := A;
      L2 (I).Bearing  := A;
      A := A + Step;
   end loop;

   Match (L1, L2, Odom, Result, Code);

   Put_Line ("Outcome: " & Code'Img);
   Put_Line ("X = " & Result.X'Img & "; " &
             "Y = " & Result.Y'Img & "; " &
             "A = " & Result.A'Img);

end Basic;
