with Player,
     Player.Types,
--       Sancta.Convert,
     Sancta.Located_Agent,
     Sancta.Player.Draw;
with Sancta.Types.Player;

package body Sancta.Ctree.Draw is

   use type Types.Real;

   ------------------------------
   -- Draw_Connectivity_Matrix --
   ------------------------------

   procedure Draw_Connectivity_Matrix
     (Drawer : in out Standard.Player.Graphics2d.Object'Class;
      Agents :        AC.Lists.List;
      Matrix :        Connectivity_Matrix.Object;
      Real   :        Connectivity_Matrix.Object;
      Skew   :        Types.Real := 0.0)
   is
      use AC.Lists;
      I : Cursor := Agents.First;
      J : Cursor;
      No_Draw : Boolean;
   begin
      while Has_Element (I) loop
         J := Agents.First;
         while Has_Element (J) loop
            if I /= J and then
              Matrix.Contains (Element (I).Get_Name, Element (J).Get_Name)
            then
              if Matrix.Is_Weak (Element (I).Get_Name, Element (J).Get_Name) then
                  if Real.Is_Weak (Element (I).Get_Name, Element (J).Get_Name) then
                     Drawer.Set_Color ((0, 255, 0, 0));
                  else
                     Drawer.Set_Color ((0, 0, 183, 0));
                  end if;
                  No_Draw := False;
               else
                  --  Draw with other color, is the tree but strong
                  Drawer.Set_Color ((0, 155, 155, 155));
                  No_Draw := True;
               end if;
               if True or else not No_Draw then
                  declare
                     A1 : constant Located_Agent.Object'Class :=
                            Located_Agent.Object'Class (Element (I));
                     A2 : constant Located_Agent.Object'Class :=
                            Located_Agent.Object'Class (Element (J));
                  begin
                     Sancta.Player.Draw.Draw_Line
                       (Drawer,
                        A1.Get_Pose.X + Skew,
                        A1.Get_Pose.Y,
                        A2.Get_Pose.X + Skew,
                        A2.Get_Pose.Y);
                  end;
               end if;
            end if;
            Next (J);
         end loop;
         Next (I);
      end loop;
   end Draw_Connectivity_Matrix;

   procedure Draw_Obstacles
     (Drawer : in out Standard.Player.Graphics2d.Object'Class;
      Obsts  :        Obstacles.Arrays.C_Array)
   is
      D : constant Float := 0.35;
      X, Y : Float;

--        function "+" (L : Standard.Player.C_Float; R : Float) return Sancta.Types.Real is
--        begin
--           return Sancta.Types.Real (L) + Sancta.Types.Real (R);
--        end "+";
--        function "-" (L : Standard.Player.C_Float; R : Float) return Sancta.Types.Real is
--        begin
--           return Sancta.Types.Real (L) - Sancta.Types.Real (R);
--        end "-";
   begin
      Drawer.Set_Color ((0, 0, 0, 0));
      for I in Obsts'Range loop
         X := Float (Obsts (I).X);
         Y := Float (Obsts (I).Y);
         Sancta.Player.Draw.Draw_Line
           (Drawer,
            X - D, Y - D,
            X + D, Y - D);
         Sancta.Player.Draw.Draw_Line
           (Drawer,
            X + D, Y - D,
            X + D, Y + D);
         Sancta.Player.Draw.Draw_Line
           (Drawer,
            X + D, Y + D,
            X - D, Y + D);
         Sancta.Player.Draw.Draw_Line
           (Drawer,
            X - D, Y + D,
            X - D, Y - D);
      end loop;
   end Draw_Obstacles;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Links;
                   G2d  : in out Standard.Player.Graphics2d.Object'Class)
   is
--        use Sancta.Convert;
      use Sancta.Types.Player;
      use Ac.Lists;
      procedure Draw (I : Cursor) is
         Bot : Located_Agent.Object'Class renames
           Located_Agent.Object'Class (Element (I));
      begin
         if
           This.Tree.Parent (Bot.Get_Name).Get_Name /= Bot.Get_Name and then
           This.Links.Is_Weak
             (Bot.Get_Name, This.Tree.Parent (Bot.Get_Name).Get_Name)
         then
            declare
               P1 : constant Types.Pose := Bot.Get_Pose;
               P2 : constant Types.Pose :=
                      This.Tree.Parent (Bot.Get_Name).Get_Pose;
               L  : constant Standard.Player.Types.Point_2d_Array :=
                      (+P1, +P2);
            begin
               G2d.Draw_Polyline (L);
            end;
         end if;
      end Draw;
   begin
      G2d.Set_Color ((0, 255, 0, 0));
      This.Tree.Get_Agents.Iterate (Draw'Access);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Action_Links_Alone;
                   G2d  : in out Standard.Player.Graphics2d.Object'Class) is
   begin
      Draw_Connectivity_Matrix (G2d,
                                This.Agents,
                                This.Scluster_Links,
                                This.Real_Links);
   end Draw;

end Sancta.Ctree.Draw;
