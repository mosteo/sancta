with Sancta.Types;

with Player.Client,
     Player.Graphics2d,
     Player.Planner,
     Player.Position2d;

package Sancta.Ctree.Robot.Player is

   Pos_Idx : Natural := 1;
   Nav_Idx : Natural := 0;

   type Object is new Robot.Object with private;

   not overriding
   procedure Connect (This : in out Object;
                      Port :        Positive := 6665;
                      Host :        String   := "127.0.0.1");

   not overriding
   procedure Iterate (This : in out Object);
   --  Update data from player, send pending commands?
   --  Set pose to the one given by player.

   not overriding
   procedure Set_Goal (This : in out Object;
                       Goal :        Types.Pose);

   not overriding
   procedure Set_Vel (This : in out Object;
                      Vel  :        Types.Pose);

   not overriding
   procedure Draw_Waypoints
     (This :        Object;
      Draw : in out Standard.Player.Graphics2d.Object'Class);

private

   package Pl renames Standard.Player;

   type Object is new Robot.Object with record
      Cli : access Pl.Client.Connection_Type := new Pl.Client.Connection_Type;
      Pos : access Pl.Position2d.Object      := new Pl.Position2d.Object;
      Nav : access Pl.Planner.Object         := new Pl.Planner.Object;

      Use_Nav   : Boolean    := True;
      Prev_Goal : Types.Pose := Types.Origin;
   end record;

end Sancta.Ctree.Robot.Player;
