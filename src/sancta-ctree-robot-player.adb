--  with Sancta.Types.Transformations;
with Sancta.Player.Draw;
with Sancta.Types.Player; use Sancta.Types.Player;

package body Sancta.Ctree.Robot.Player is

   use Sancta.Types;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (This : in out Object;
      Port :        Positive := 6665;
      Host :        String   := "127.0.0.1")
   is
   begin
      This.Cli.Create    (Host, Port);
      This.Cli.Connect;
      This.Cli.Datamode  (Pl.Client.Datamode_Pull);
      This.Cli.Set_Replace_Rule (-1, -1, -1, -1, True);
      This.Pos.Create    (This.Cli.all, Pos_Idx);
      This.Pos.Subscribe (Pl.Open_Mode);
      This.Nav.Create    (This.Cli.all);
      begin
         This.Nav.Subscribe (Pl.Open_Mode);
      exception
         when others =>
            This.Use_Nav := False;
      end;
   end Connect;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This : in out Object)
   is
   begin
      This.Cli.Read;
      This.Set_Pose (+This.Pos.Get_Pose);
   end Iterate;

   --------------
   -- Set_Goal --
   --------------

   procedure Set_Goal
     (This : in out Object;
      Goal :        Types.Pose)
   is
   begin
      if This.Use_Nav then
         if Goal /= This.Prev_Goal then
            This.Prev_Goal := Goal;
            This.Nav.Set_Cmd_Pose (+Goal.X, +Goal.Y, +Real (Goal.A));
         end if;
--           Log ("Waipoints:" & This.Nav.Get_Waypoint_Count'Img, Always);
--           Log ("At:" & This.Nav.Get_Current_Waypoint_Index'Img, Always);
      else
         This.Prev_Goal := Goal;
         This.Pos.Set_Cmd_Pose (+Goal.X, +Goal.Y, +Real (Goal.A));
      end if;
   end Set_Goal;

   -------------
   -- Set_Vel --
   -------------

   procedure Set_Vel (This : in out Object;
                      Vel  :        Types.Pose)
   is
   begin
      This.Pos.Set_Cmd_Vel (+Vel.X, +Vel.Y, +Real (Vel.A));
   end Set_Vel;

   procedure Draw_Waypoints (This :        Object;
                             Draw : in out Pl.Graphics2d.Object'Class)
   is
      use Sancta.Player.Draw;
   begin
      if This.Use_Nav then
         null;
         pragma Incomplete ("Planner not yet in player3_ada");
--           declare
--              Points : constant Pl.Pose_Array := This.Nav.Get_Waypoints;
--              First  : constant Natural       :=
--                This.Nav.Get_Current_Waypoint_Index;
--           begin
--              if First in Points'Range then
--                 Draw_Line (Draw, This.Get_Pose, +Points (First));
--                 for I in First .. Points'Last - 1 loop
--                    Draw_Line (Draw, +Points (I), +Points (I + 1));
--                 end loop;
--              end if;
--           end;
      else
         Draw_Line (Draw, This.Get_Pose, This.Prev_Goal);
      end if;
   end Draw_Waypoints;

end Sancta.Ctree.Robot.Player;
