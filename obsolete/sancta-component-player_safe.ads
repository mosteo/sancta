with Sancta.Types;

with Agpl.Chronos;

with Player.Client;
with Player.Gps;
with Player.Graphics2d;
with Player.Laser;
with Player.Position2d;
with Player.Simulation;
with Player.Types;

--  Safe access for multithreading to the Player data.
--  This is not a component but supports the ones using player.

private package Sancta.Component.Player_Safe is

   use type Types.Angle;
   Pi : Types.Angle renames Types.Angle_Pi;

   protected Object is

      procedure Connect (Address : in String; Port : in Positive);
      procedure Disconnect;
      --  Creates the client.

      procedure Connect_Position2d_0;
      --  The first position2d interface.
      --  Used for current pose and navigation.

      procedure Connect_Laser_0;
      function  Get_Laser_Scan (From : in Types.Angle := -Pi;
                                To   : in Types.Angle :=  Pi)
                                return    Types.Range_Scan;
      function  Get_Laser_Scan_Id return Integer;
      function  Get_Laser_Pose return Types.Pose;
      --  These require a laser:0 interface

      procedure Connect_Simulation_0;
      function  Get_Pose (Id : in String) return Types.Pose;
      --  Truth pose from simulator

      function  Get_Pose return Types.Pose;
      --  This requires a position2d:0 interface

      function  Get_Size return Types.Pose;

      procedure Goto_Pose (Pose : in Types.Pose; Avoid : in Boolean := True);
      function  Get_Vel return Types.Pose;
      procedure Set_Vel (X, Y, A : Types.Real);
      --  These three require a position2d:0 interface

      procedure Connect_Gps_0;
      --  Requires gps:0
      function Get_Gps_Pose return Types.Pose;
      function Get_Gps_Seconds return Duration;

      procedure Connect_Graphics2d_0;
      --  Requires graphics2d:0
      procedure Graphics_Clear;
      procedure Graphics_Set_Color (Color : Player.Types.Player_Color_Type);
      procedure Graphics_Draw_Polyline (Points : Player.Types.Point_2d_Array);

      procedure Emergency_Stop;

      procedure Poll_Player;

   private

      Client  : Player.Client.Connection_Type;
      Laser   : Player.Laser.Object;
      Pos0    : Player.Position2d.Object;
      Gps     : Player.Gps.Object;         --  Gps feed.
      Sim     : Player.Simulation.Object;
      Draw    : Player.Graphics2d.Object;

      Pad     : Agpl.Chronos.Object; -- We will issue a warning if too many data
                                     --  is feed to player in short time.
   end Object;

end Sancta.Component.Player_Safe;
