with Sancta.Ctree.Connectivity_Matrix,
     Sancta.Ctree.Obstacles,
     Sancta.Ctree.Team_Tree,

     Player.Graphics2d,

     Sancta.Agent.Containers,
     Sancta.Component.Player_Graphics2d,
     Sancta.Types;

use  Sancta;

package Sancta.Ctree.Draw is

   package AC renames Sancta.Agent.Containers;

   procedure Draw_Connectivity_Matrix
     (Drawer : in out Player.Graphics2d.Object'Class;
      Agents :        AC.Lists.List;
      Matrix :        Connectivity_Matrix.Object;
      Real   :        Connectivity_Matrix.Object;
      Skew   :        Types.Real := 0.0);
   --  Matrix keeps artificially hysteresis links
   --  Real only contains the current real links
   --  Skew is used to move the drawing sideways and compare it to the real one.

   procedure Draw_Obstacles
     (Drawer : in out Player.Graphics2d.Object'Class;
      Obsts  :        Obstacles.Arrays.C_Array);

   subtype Action is Component.Player_Graphics2d.Actions;

   type Action_Links is new Action with record
      Links : Connectivity_Matrix.Object;
      Tree  : Ctree.Team_Tree.Object;
   end record;
   --  For links and a given team tree

   type Action_Links_Alone is new Action with record
      Agents : Ac.Lists.List;
      Real_Links     : Connectivity_Matrix.Object;
      Scluster_Links : Connectivity_Matrix.Object;
   end record;
   --  For links

private

   overriding
   procedure Draw (This :        Action_Links;
                   G2d  : in out Player.Graphics2d.Object'Class);

   overriding
   procedure Draw (This :        Action_Links_Alone;
                   G2d  : in out Player.Graphics2d.Object'Class);

end Sancta.Ctree.Draw;
