with Sancta.Assignment,
     Sancta.Located_Agent,
     Sancta.Map,
     Sancta.Tasks.Containers,
     Sancta.Types;

with Player.Graphics2d;
with Player.Types;

package Sancta.Player.Draw is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.player.draw";

   package PG renames Standard.Player.Graphics2d;

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        X1, Y1,
                        X2, Y2 : Types.Real); pragma Inline (Draw_Line);

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        X1, Y1,
                        X2, Y2 : Float);      pragma Inline (Draw_Line);

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        P1, P2 : Types.Pose); pragma Inline (Draw_Line);

   procedure Draw_Goals (Drawer : in out PG.Object'Class;
                         Tasks  :        Sancta.Tasks.Containers.Lists.List);
   --  Drawer color will be altered.
   --  Tasks must be of Positioned ancestry!

   procedure Draw_Plan (Drawer : in out PG.Object'Class;
                        Ass    :        Sancta.Assignment.Object;
                        Direct :        Boolean := False);
   --  If Direct is true, a straight line will be drawn between bot and goal,
   --   even if the task is a complex Goto with multiple waypoints.
   --  Otherwise, only the waypoints path is drawn.

   procedure Draw_Plan (Drawer : in out Pg.Object'Class;
                        Tasks  :        Sancta.Tasks.Containers.Lists.List);

   procedure Draw_Path
     (Drawer : in out Pg.Object'Class;
      Map    :        Sancta.Map.Object'Class;
      Path   :        Sancta.Map.Path;
      Color  :        Standard.Player.Types.Player_Color_Type);

   procedure Draw_Complex_Goto_Pose
     (Drawer : in out Pg.Object'Class;
      Bot    :        Located_Agent.Object'Class;
      Job    :        Sancta.Tasks.Object'Class;
      Color  :        Standard.Player.Types.Player_Color_Type);

end Sancta.Player.Draw;
