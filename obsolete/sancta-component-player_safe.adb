with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Types; use Sancta.Types;

with Player;
with Player.Exceptions;

with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Component.Player_Safe is

   protected body Object is

      -------------
      -- Connect --
      -------------

      procedure Connect (Address : in String; Port : in Positive) is
      begin
         Player.Client.Create   (Client, Address, Port);
         Player.Client.Connect  (Client);
         Player.Client.Datamode (Client, Player.Client.Datamode_Pull);
         Player.Client.Set_Replace_Rule
           (Client, -1, -1, Player.Msgtype_Data, -1, True);
         Player.Client.Set_Request_Timeout (Client, 60);
         Log ("Connected with player successfully", Informative);
      end Connect;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         Player.Client.Disconnect (Client);
      end Disconnect;

      --------------------------
      -- Connect_Position2d_0 --
      --------------------------

      procedure Connect_Position2d_0 is
      begin
         Player.Position2d.Create    (Pos0, Client);
         Player.Position2d.Subscribe (Pos0, Player.Open_Mode);
         Player.Position2d.Enable    (Pos0);
         --  This call will hang the second time if Pos0 is vfh (with real robots)
         Log ("Connected with Position2d successfully", Informative);
      end Connect_Position2d_0;

      ---------------------
      -- Connect_Laser_0 --
      ---------------------

      procedure Connect_Laser_0 is
      begin
         Player.Laser.Create    (Laser, Client);
         Player.Laser.Subscribe (Laser, Player.Open_Mode);
         Log ("Connected with Laser successfully", Informative);
      end Connect_Laser_0;

      --------------------
      -- Get_Laser_Scan --
      --------------------

      function Get_Laser_Scan (From : in Types.Angle := -Pi;
                               To   : in Types.Angle :=  Pi)
                               return    Types.Range_Scan
      is
         use Player;
         use Player.Laser;
         Scan  : Types.Range_Scan (1 .. Get_Scan_Count (Laser));
         Idx   : Positive := Scan'First;
      begin
         for I in 1 .. Scan'Length loop
            if Get_Bearing (Laser, I) >= Player.Double (From) and then
               Get_Bearing (Laser, I) <= Player.Double (To)
            then
               Scan (Idx) :=
                 (A  =>   Types.Angle (Get_Bearing (Laser, I)),
                  D  => + Get_Range (Laser, I));
               Idx := Idx + 1;
            end if;
         end loop;

         return Scan (Scan'First .. Idx - 1);
      end Get_Laser_Scan;

      -----------------------
      -- Get_Laser_Scan_Id --
      -----------------------

      function Get_Laser_Scan_Id return Integer is
      begin
         return Player.Laser.Get_Scan_Id (Laser);
      end Get_Laser_Scan_Id;

      --------------------
      -- Get_Laser_Pose --
      --------------------

      function Get_Laser_Pose return Types.Pose is
      begin
         return +Player.Laser.Get_Pose (Laser);
      end Get_Laser_Pose;

      --------------------------
      -- Connect_Simulation_0 --
      --------------------------

      procedure Connect_Simulation_0 is
      begin
         Player.Simulation.Create    (Sim, Client);
         Player.Simulation.Subscribe (Sim, Player.Open_Mode);
      end Connect_Simulation_0;

      --------------
      -- Get_Pose --
      --------------

      function Get_Pose (Id : in String) return Types.Pose is
         P : constant Player.Pose := Player.Simulation.Get_Pose2d (Sim, Id);
      begin
         return (+P (1), +P (2), Types.Angle (P (3)));
      end Get_Pose;

      --------------
      -- Get_Pose --
      --------------
      function Get_Pose return Types.Pose is
         P : constant Player.Pose := Player.Position2d.Get_Pose (Pos0);
      begin
         return (+P (1), +P (2), Types.Angle (P (3)));
      end Get_Pose;

      --------------
      -- Get_Size --
      --------------

      function Get_Size return Types.Pose is
         W, H : Player.Double;
      begin
         Player.Position2d.Get_Size (Pos0, W, H);
         return (+W, +H, 0.0);
      end Get_Size;

      ---------------
      -- Goto_Pose --
      ---------------

      procedure Goto_Pose (Pose : in Types.Pose; Avoid : in Boolean := True) is
         pragma Unreferenced (Avoid);
         pragma Unimplemented (Avoid);
         use type Player.Double;
      begin
--         Log ("Going to pose " & To_String (Pose), Always);
         --  Introduce a slight noise in angle to prevent stuckments due to
         --  the particular player/stage implementation:
         --  Move to X / Stop / Move to Y will not work if X = Y!

         Player.Position2d.Set_Cmd_Pose (Pos0,
                                         Player.Double (Pose.X) +
                                         Player.Double (Random.Get_Float (0.0, 0.001)),
                                         Player.Double (Pose.Y) +
                                         Player.Double (Random.Get_Float (0.0, 0.001)),
                                         Player.Double (Pose.A) +
                                         Player.Double (Random.Get_Float (0.0, 0.01)));
      end Goto_Pose;

      -------------
      -- Get_Vel --
      -------------

      function Get_Vel return Types.Pose is
         P : constant Player.Pose := Player.Position2d.Get_Velocity (Pos0);
      begin
         return (+P (1), +P (2), Types.Angle (P (3)));
      end Get_Vel;

      -------------
      -- Set_Vel --
      -------------

      procedure Set_Vel (X, Y, A : Types.Real) is
      begin
--         Log ("Setting vel " & To_String (Types.Pose'(X, Y, Types.Angle (A))), Always);
         Player.Position2d.Set_Cmd_Vel (Pos0,
                                        Player.Double (X),
                                        Player.Double (Y),
                                        Player.Double (A));
      end Set_Vel;

      -------------------
      -- Connect_Gps_0 --
      -------------------

      procedure Connect_Gps_0 is
      begin
         Player.Gps.Create    (Gps, Client);
         Player.Gps.Subscribe (Gps, Player.Open_Mode);
      end Connect_Gps_0;

      ------------------
      -- Get_Gps_Pose --
      ------------------

      function Get_Gps_Pose return Types.Pose is
      begin
         return +Player.Gps.Get_Pose (Gps);
      end Get_Gps_Pose;

      ---------------------
      -- Get_Gps_Seconds --
      ---------------------

      function Get_Gps_Seconds return Duration is
      begin
         return Player.Gps.Get_Seconds (Gps);
      end Get_Gps_Seconds;

      --------------------------
      -- Connect_Graphics2d_0 --
      --------------------------

      procedure Connect_Graphics2d_0 is
      begin
         Player.Graphics2d.Create    (Draw, Client);
         Player.Graphics2d.Subscribe (Draw, Player.Open_Mode);
      end Connect_Graphics2d_0;

      --------------------
      -- Graphics_Clear --
      --------------------

      procedure Graphics_Clear is
      begin
         Draw.Clear;
      end Graphics_Clear;

      ------------------------
      -- Graphics_Set_Color --
      ------------------------

      procedure Graphics_Set_Color (Color : Player.Types.Player_Color_Type) is
      begin
         Draw.Set_Color (Color);
      end Graphics_Set_Color;

      ----------------------------
      -- Graphics_Draw_Polyline --
      ----------------------------

      procedure Graphics_Draw_Polyline (Points : Player.Types.Point_2d_Array) is
      begin
         Draw.Draw_Polyline (Points);
      end Graphics_Draw_Polyline;

      --------------------
      -- Emergency_Stop --
      --------------------

      procedure Emergency_Stop is
      begin
         null;
      end Emergency_Stop;

      -----------------
      -- Poll_Player --
      -----------------

      procedure Poll_Player is
         Timer : Chronos.Object;
      begin
         Log ("READING FROM PLAYER, OBSOLETE!", Always);
         Player.Client.Read (Client);
         if Timer.Elapsed >= 2.0 then
            Log ("Player_Safe: " &
                 "Anormal delay from player [too much data or others]!",
                 Warning);
         end if;
         null;
      exception
         when E : Player.Exceptions.Player_Error =>
            Log ("Player_Safe.Poll_Player: " & Report (E), Warning);
            --  raise;
      end Poll_Player;

   end Object;

end Sancta.Component.Player_Safe;
