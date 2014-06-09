with Sancta.Agent_Proxy;
with Sancta.Constants;
with Sancta.Containers;
with Sancta.Tasks.Complex_Goto_Pose;
with Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;
with Sancta.Tasks.Positioned;
with Sancta.Types.Player; use Sancta.Types.Player;

with Sancta.Agent.Containers;

package body Sancta.Player.Draw is

   package AC renames Sancta.Agent.Containers;
   package PT renames Standard.Player.Types;
   package TC renames Sancta.Tasks.Containers;

   package Navigate renames Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;

   function "+" (C    : Sancta.Types.Colors)
                 return Standard.Player.Types.Player_Color_Type
   is
      use Standard.Player.Types;
   begin
      return (0, Uint8 (C.R), Uint8 (C.G), Uint8 (C.B));
   end "+";

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        X1, Y1,
                        X2, Y2 : Types.Real) is
   begin
      Draw_Line (Drawer, Float (X1), Float (Y1), Float (X2), Float (Y2));
   end Draw_Line;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        X1, Y1,
                        X2, Y2 : Float)
   is
      Line : constant PT.Point_2d_Array (1 .. 2) :=
        ((+X1, +Y1),
         (+X2, +Y2));
   begin
      Drawer.Draw_Polyline (Line);
   end Draw_Line;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Drawer : in out PG.Object'Class;
                        P1, P2 : Types.Pose)
   is
   begin
      Draw_Line (Drawer, P1.X, P1.Y, P2.X, P2.Y);
   end Draw_Line;

   ----------------
   -- Draw_Goals --
   ----------------

   procedure Draw_Goals
     (Drawer : in out PG.Object'Class;
      Tasks  :        Sancta.Tasks.Containers.Lists.List)
   is
      use Sancta.Types;
      use Standard.Player.Types;
      use Sancta.Tasks.Containers.Lists;
      I : Cursor := Tasks.First;
   begin
      Drawer.Set_Color ((0, 0, 0, 0));

      while Has_Element (I) loop
         declare
            use type Pt.Player_Float;
            Size : constant := 0.8;
            G  : constant Sancta.Tasks.Positioned.Object :=
                   Sancta.Tasks.Positioned.Object (Element (I));
            L1 : constant PT.Point_2d_Array (1 .. 2) :=
                   ((+(G.Pose.X - Size), +(G.Pose.Y - Size)),
                    (+(G.Pose.X + Size), +(G.Pose.Y + Size)));
            L2 : constant PT.Point_2d_Array (1 .. 2) :=
                   ((+(G.Pose.X - Size), +(G.Pose.Y + Size)),
                    (+(G.Pose.X + Size), +(G.Pose.Y - Size)));
         begin
            Drawer.Draw_Polyline (L1);
            Drawer.Draw_Polyline (L2);
         end;
         Next (I);
      end loop;
   end Draw_Goals;

   ---------------
   -- Draw_Path --
   ---------------

   procedure Draw_Path
     (Drawer : in out Pg.Object'Class;
      Map    :        Sancta.Map.Object'Class;
      Path   :        Sancta.Map.Path;
      Color  :        Standard.Player.Types.Player_Color_Type)
   is
      Prev,
      Curr  : Types.Pose;
      Route : constant Sancta.Containers.Pose_Vectors.Vector :=
                Map.To_Poses (Path);
   begin
      if not Path.Is_Empty then
         Prev := Route.First_Element;
         Drawer.Set_Color (Color);

         for I in Route.First_Index + 1 .. Route.Last_Index loop
            Curr := Route.Element (I);
            declare
               Line : constant Pt.Point_2d_Array (1 .. 2) :=
                        ((+Curr.X, +Curr.Y),
                         (+Prev.X, +Prev.Y));
            begin
               Drawer.Draw_Polyline (Line);
               Prev := Curr;
            end;
         end loop;
      end if;
   end Draw_Path;

   ----------------------------
   -- Draw_Complex_Goto_Pose --
   ----------------------------

   procedure Draw_Complex_Goto_Pose
     (Drawer : in out Pg.Object'Class;
      Bot    :        Located_Agent.Object'Class;
      Job    :        Sancta.Tasks.Object'Class;
      Color  :        Standard.Player.Types.Player_Color_Type)
   is
      T     : Navigate.Object renames Navigate.Object (Job);
      Prev  : Types.Pose := Bot.Get_Pose;
      Curr  : Types.Pose := T.To_Goto_Pose.Pose;
      Route : constant Sancta.Containers.Pose_Vectors.Vector :=
                T.Get_Map.Ref.all.To_Vector (T.Get_Route);
   begin
      declare
         Line : constant Pt.Point_2d_Array (1 .. 2) :=
                  ((+(Curr.X), +(Curr.Y)),
                   (+(Prev.X), +(Prev.Y)));
      begin
         Drawer.Set_Color (Color);
         Drawer.Draw_Polyline (Line);
         Prev := Curr;
      end;

      for I in Route.First_Index + 2 .. Route.Last_Index loop
         Curr := Route.Element (I);
         declare
            Line : constant Pt.Point_2d_Array (1 .. 2) :=
                     ((+(Curr.X), +(Curr.Y)),
                      (+(Prev.X), +(Prev.Y)));
         begin
            Drawer.Draw_Polyline (Line);
            Prev := Curr;
         end;
      end loop;
   end Draw_Complex_Goto_Pose;

   ---------------
   -- Draw_Plan --
   ---------------

   procedure Draw_Plan (Drawer : in out PG.Object'Class;
                        Ass    :        Sancta.Assignment.Object;
                        Direct :        Boolean := False)
   is
      procedure Draw_Tasks (A : Sancta.Agent.Object'Class;
                            T : TC.Lists.List) is
         Prev, Curr : Types.Pose;
         I          : TC.Lists.Cursor := T.First;
      begin
         --  First goal in gray
--         Drawer.Set_Color ((0, 128, 128, 128));
         if not (A in Located_Agent.Object'Class) then
            Log ("Unlocated agent, not drawing...", Debug, Log_Section);
            return;
         else
            Prev := Located_Agent.Object'Class (A).Get_Pose;
         end if;
         while Tc.Lists.Has_Element (I) loop
            if Direct or else
              (TC.Lists.Element (I) in Tasks.Positioned.Object'Class and then
               Tc.Lists.Element (I) not in Tasks.Complex_Goto_Pose.Object'Class)
            then
               Curr := Tasks.Positioned.Object (TC.Lists.Element (I)).Pose;

               declare
                  Line : constant PT.Point_2d_Array (1 .. 2) :=
                   ((+(Curr.X), +(Curr.Y)),
                    (+(Prev.X), +(Prev.Y)));
               begin
                  Drawer.Set_Color ((0, 128, 128, 255));
                  Drawer.Draw_Polyline (Line);
--                Drawer.Set_Color ((0, 128, 128, 255));
                  --  After first line, switch to blue for plan
               end;

               Prev := Curr;
            end if;

            --  Extra draw for complex_goto_pose tasks
            if Tc.Lists.Element (I) in Tasks.Complex_Goto_Pose.Object'Class then
               Draw_Complex_Goto_Pose
                 (Drawer,
                  Located_Agent.Object'Class (A),
                  Navigate.Object (Tc.Lists.Element (I)),
                  +Constants.Agent_Colors (Integer'Value (A.Get_Name)));
            end if;

            Tc.Lists.Next (I);
         end loop;
      end Draw_Tasks;

      procedure Draw_Agent (I : AC.Lists.Cursor) is
      begin
         Draw_Tasks (AC.Lists.Element (I),
                     AC.Lists.Element (I).Get_Tasks);
      end Draw_Agent;
   begin
      Drawer.Set_Color ((0, 128, 128, 255));
      Ass.Get_Agents.Iterate (Draw_Agent'Access);
   end Draw_Plan;

   ---------------
   -- Draw_Plan --
   ---------------

   procedure Draw_Plan (Drawer : in out Pg.Object'Class;
                        Tasks  :        Sancta.Tasks.Containers.Lists.List)
   is
      Ass : Sancta.Assignment.Object;
      Bot : Sancta.Agent_Proxy.Object;
   begin
      if not Tasks.Is_Empty then
         Bot.Set_Pose
           (Sancta.Tasks.Positioned.Object (Tasks.First_Element).Pose);
         Bot.Set_Tasks (Tasks);
         Ass.Set_Agent (Bot);
         Draw_Plan (Drawer, Ass);
      end if;
   end Draw_Plan;

end Sancta.Player.Draw;
