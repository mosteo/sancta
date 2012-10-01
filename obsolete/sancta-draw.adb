with Sancta.Gui.Visor_Data;
with Sancta.Agent_Proxy;
with Sancta.Tasks.Explore_Edge;
with Sancta.Tasks.Explore_Directed_Edge;
with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Grid_Goal;
--  with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Positioned;
with Sancta.Types.Transformations;

with Agpl.Conversions; use Agpl.Conversions;
with Sancta;
with Sancta.Agent.Containers;
with Sancta.Cost_Cache.Handle;
with Agpl.Gdk.Drawer_Arc;
with Agpl.Gdk.Drawer_Figures;
with Agpl.Gdk.Drawer_Point;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Gdk.Modal_Drawing_Area;
with Sancta.Tasks.Containers;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;
with Gdk.Window;   use Gdk.Window; -- For Set_Background
with Glib;         use Glib;
with Pango.Layout; use Pango.Layout;

package body Sancta.Draw is

   use type Types.Real;

--     type Draw_Assignment_Code is new Agpl.Gdk.Managed.Drawing_Area.Draw_Code with
--        record
--           A     :         Sancta.Assignment.Object;
--           Pal   : aliased Agpl.Gdk.Palette.Object;
--           Costs :         Sancta.Cost_Cache.Handle.Object;
--           Show  :         Boolean := True;
--        end record;
--
--     -------------
--     -- Execute --
--     -------------
--
--     procedure Execute (This : in out Draw_Assignment_Code)
--     is
--        use Agpl.Gdk.Palette;
--        use Agpl.Gdk.Drawer;
--        use Agpl.Gdk.Drawer_Figures;
--        W : constant Gdk_Drawable := Drawable (This);
--        D : Agpl.Gdk.Drawer.Object;
--
--        White : constant String := "#ffffff";
--     begin
--        Set_Drawable (This.Pal, W);
--        D.Set_Drawable (W);
--        Draw_Begin (D);
--
--        Set_Background (W, Get_Color (This.Pal'Access, White));
--        Clear (W);
--
--        Draw_Assignment (This.A,
--                         D,
--                         This.Pal'Access,
--                         This.Widget,
--                         This.Costs.Ref.all,
--                         This.Show);
--
--        Draw_End (D);
--     end Execute;

   ---------------------
   -- Draw_Assignment --
   ---------------------

   procedure Draw_Assignment
     (Ass        : in     Sancta.Assignment.Object;
      D          : in out Agpl.Gdk.Drawer.Object;
      Pal        : not null access Agpl.Gdk.Palette.Object;
      Widget     : in     Gtk_Widget;
      Costs      : in     Sancta.Cost_Cache.Object'Class := Sancta.Cost_Cache.Empty_Object;
      Show_Costs : in     Boolean := True)
   is
      use Agpl.Gdk.Palette;
      use Agpl.Gdk.Drawer;
      use Agpl.Gdk.Drawer_Figures;
      W : constant Gdk_Drawable := D.Get_Drawable;

      Total_Cost : Sancta.Costs := 0.0;
      Worst_Cost : Sancta.Costs := 0.0;

      Black : constant String := "#000000";
   begin

      --        --  Draw origin:
      --        Draw_Plus (0.0, 0.0, Get_Color (This.Pal'Access, Gray), D);
      --        --  Draw some grid for scale awareness:
      --        Draw_Grid_Points (-5.0, -5.0, 5.0,  5.0,
      --                          Get_Color (This.Pal'Access, Gray), D);

      --  Draw robots
      declare
         Bots : constant Sancta.Agent.Containers.Lists.List   := Ass.Get_Agents;
         I    :          Sancta.Agent.Containers.Lists.Cursor := Bots.First;
         use Sancta.Agent.Containers.Lists;

         Text_Row   : Gint := 30;

         use type Sancta.Costs;
      begin
         while Has_Element (I) loop
            declare
               Bot       : Sancta.Agent.Object'Class renames Element (I);
               Bot_Color : constant String := Gui.Visor_Data.Get_Robot_Color (Bot.Get_Name);
               Plan_Cost : constant Sancta.Costs :=
                             Sancta.Cost_Cache.Get_Plan_Cost (Costs, Bot);
               Bot_Pose  : Types.Pose;
               Bot_Posed : Boolean;
            begin
               Bot_Posed := Bot in Agent_Proxy.Object'Class;
               if Bot_Posed then
                  Bot_Pose := Agent_Proxy.Object (Bot).Get_Pose;
               end if;

               if Plan_Cost = Sancta.Infinite or else
                 Sancta.Infinite - Total_Cost < Plan_Cost
               then
                  Total_Cost := Sancta.Infinite;
               else
                  Total_Cost := Total_Cost + Plan_Cost;
                  Worst_Cost := Sancta.Costs'Max (Worst_Cost, Plan_Cost);
               end if;

               if Show_Costs then
                  --  Textual info about cost:
                  declare
                     Cost_Layout : constant Pango_Layout :=
                                     Create_Pango_Layout
                                       (Widget,
                                        Bot.Get_Name & " cost:"
                                        & To_String (Float (Plan_Cost)));
                  begin
                     Draw_Layout (W, Get_Color (Pal, Bot_Color),
                                  10, Text_Row, Cost_Layout);
                     Text_Row := Text_Row + 10;
                  end;
               end if;

               --  Draw tasks
               --  We use Prev_Pose to store a meaningful previous pose.
               --  We use Use_Pose to signal if Prev_Pose is valid
               declare
                  use Sancta.Tasks.Containers.Lists;
                  Tasks     : constant Sancta.Tasks.Containers.Lists.List :=
                                Ass.Get_Tasks (Bot);
                  Prev_Pose : Types.Pose    := Bot_Pose;
                  Use_Pose  : Boolean       := Bot_Posed;
                  T         : Sancta.Tasks.Containers.Lists.Cursor;
               begin
                  T := Tasks.First;

                  while Has_Element (T) loop
                     if Element (T) in Sancta.Tasks.Positioned.Object'Class then
                        declare
                           New_Pose : Types.Pose renames
                             Sancta.Tasks.Positioned.Object (Element (T)).Pose;
                           --  Goal_Size : constant Float := 0.2;
                        begin
                           --  Joining path
                           if Use_Pose then
                              Agpl.Gdk.Drawer_Figures.Draw_Segment
                                (+Prev_Pose.X, +Prev_Pose.Y, +New_Pose.X, +New_Pose.Y,
                                 Get_Gc (Pal,
                                   Bot_Color,
                                   Line_Style => Line_On_Off_Dash), D);
                           end if;
                           Prev_Pose := New_Pose;
                           Use_Pose  := True;

                           --  Bot at goal
--                             Draw_Robot (+New_Pose.X,
--                                         +New_Pose.Y,
--                                         Float (New_Pose.A),
--                                         Get_Color (Pal, Bot_Color), D);
                           D.Draw
                             (Agpl.Gdk.Drawer_Arc.Create_Circle
                                (Get_Color (Pal, Bot_Color),
                                 +New_Pose.X,
                                 +New_Pose.Y, 0.5));
                        end;
                     elsif Element (T) in Sancta.Tasks.Explore_Directed_Segment.Object'Class then
                        declare
                           Segment   : Sancta.Tasks.Explore_Directed_Segment.Object renames
                             Sancta.Tasks.Explore_Directed_Segment.Object (Element (T));
                           Join_Pose : Types.Pose;
                        begin
                           --  Straight Path (arrow):
                           Agpl.Gdk.Drawer_Figures.Draw_Arrow
                             (+Segment.Get_From.X, +Segment.Get_From.Y,
                              +Segment.Get_To.X, +Segment.Get_To.Y,
                              0.5,
                              Get_Gc (Pal,
                                Bot_Color,
                                Line_Width => 3), D);

                           --  Join with previous pose
                           if Use_Pose then
                              if Segment.On_Segment then
                                 Join_Pose := Segment.Get_To;
                              else
                                 Join_Pose := Segment.Get_From;
                              end if;

                              Agpl.Gdk.Drawer_Figures.Draw_Segment
                                (+Join_Pose.X, +Join_Pose.Y,
                                 +Prev_Pose.X, +Prev_Pose.Y,
                                 Get_Gc (Pal,
                                   Bot_Color,
                                   Line_Style => Line_On_Off_Dash), D);
                           end if;

                           Prev_Pose := Segment.Get_To;
                           Use_Pose  := True;
                        end;
                     elsif Element (T) in Sancta.Tasks.Explore_Directed_Edge.Object'Class then
                        declare
                           Segment   : Sancta.Tasks.Explore_Directed_Edge.Object renames
                             Sancta.Tasks.Explore_Directed_Edge.Object (Element (T));
                           Join_Pose : Types.Pose;
                           use Sancta.Tasks.Explore_Edge;
                           use Sancta.Tasks.Explore_Directed_Edge;
                           Pose_Ini  : Types.Pose renames Get_Pose (Get_From (Segment));
                           Pose_Fin  : Types.Pose renames Get_Pose (Get_To   (Segment));
                        begin
                           --  Straight Path (arrow):
                           Agpl.Gdk.Drawer_Figures.Draw_Arrow
                             (+Pose_Ini.X, +Pose_Ini.Y,
                              +Pose_Fin.X, +Pose_Fin.Y,
                              0.5,
                              Get_Gc (Pal,
                                Bot_Color,
                                Line_Width => 3), D);

                           --  Join with previous pose
                           if Use_Pose then
                              if Segment.On_Segment then
                                 Join_Pose := Pose_Fin;
                              else
                                 Join_Pose := Pose_Ini;
                              end if;

                              Agpl.Gdk.Drawer_Figures.Draw_Segment
                                (+Join_Pose.X, +Join_Pose.Y,
                                 +Prev_Pose.X, +Prev_Pose.Y,
                                 Get_Gc (Pal,
                                   Bot_Color,
                                   Line_Style => Line_On_Off_Dash), D);
                           end if;

                           Prev_Pose := Pose_Fin;
                           Use_Pose  := True;
                        end;
                     elsif Element (T) in Sancta.Tasks.Grid_Goal.Object then
                        declare
                           Goal      : Sancta.Tasks.Grid_Goal.Object renames
                             Sancta.Tasks.Grid_Goal.Object (Element (T));
                           Pose_Fin  : constant Types.Pose := Goal.Get_Pose;
                        begin
                           --  Join with previous pose
                           if Use_Pose then
                              Agpl.Gdk.Drawer_Figures.Draw_Segment
                                (+Pose_Fin.X, +Pose_Fin.Y,
                                 +Prev_Pose.X, +Prev_Pose.Y,
                                 Get_Gc (Pal,
                                   Bot_Color,
                                   Line_Style => Line_On_Off_Dash), D);
                           end if;

                           Prev_Pose := Pose_Fin;
                           Use_Pose  := True;
                        end;
                     else
                        Use_Pose := False;
                     end if;

                     Next (T);
                  end loop;
               end;
            end;
            Next (I);
         end loop;
      end;

      if Show_Costs then
         --  Textual info about plan costs:
         declare
            Cost_Layout : constant Pango_Layout :=
                            Create_Pango_Layout
                              (Widget,
                               "MinSum cost:" & To_String (Float (Total_Cost)));
         begin
            Draw_Layout (W, Get_Color (Pal, Black),
                         10, 10, Cost_Layout);
         end;
         declare
            Cost_Layout : constant Pango_Layout :=
                            Create_Pango_Layout
                              (Widget,
                               "MinMax cost:" & To_String (Float (Worst_Cost)));
         begin
            Draw_Layout (W, Get_Color (Pal, Black),
                         10, 20, Cost_Layout);
         end;
      end if;
   end Draw_Assignment;

   ---------------------
   -- Draw_Assignment --
   ---------------------

--     procedure Draw_Assignment
--       (This       : in Sancta.Assignment.Object;
--        Costs      : in Sancta.Cost_Cache.Object'Class := Sancta.Cost_Cache.Empty_Object;
--        Show_Costs : in Boolean := True)
--     is
--        Area : Draw_Assignment_Code;
--     begin
--        Area.A     := This;
--        Area.Costs.Set (Costs);
--        Area.Show  := Show_Costs;
--
--        Agpl.Gdk.Managed.Drawing_Area.Show (Area, "Assignment");
--     end Draw_Assignment;

   type Draw_Laser_Data is new Agpl.Gdk.Modal_Drawing_Area.Callback with
      record
         Scan      : Types.Posed_Range_Scan_Vectors.Vector;
         Max_Range : Types.Real;
      end record;

   overriding
   procedure Draw (This     : in out Draw_Laser_Data;
                   Drawable :        Gdk_Drawable);

   ----------
   -- Draw --
   ----------

   procedure Draw (This     : in out Draw_Laser_Data;
                   Drawable :        Gdk_Drawable)
   is
      use Agpl.Gdk;
      use Sancta.Types.Transformations;
      use Sancta.Types.Transformations.Real_Transf;

      D   : Drawer.Object;
      Pal : aliased Palette.Object;
   begin
      Set_Background (Drawable,
                      Agpl.Gdk.Palette.Get_Color (Pal, "white"));
      Clear (Drawable);

      D.Set_Drawable (Drawable);
      Pal.Set_Drawable (Drawable);

      D.Draw_Begin;

      for I in This.Scan.First_Index .. This.Scan.Last_Index loop
         declare
            Bot  : Types.Pose renames This.Scan.Element (I).Pose;
            Scan : Types.Range_Scan renames This.Scan.Element (I).Scan;
         begin
            Drawer_Figures.Draw_Robot (+Bot.X, +Bot.Y, Float (Bot.A),
              Palette.Get_Gc (Pal'Unrestricted_Access,
                "red"),
              D);
            for I in Scan'Range loop
               declare
                  P    : constant Types.Pose := Polar_To_Cart (Scan (I).A,
                                                               Scan (I).D);
                  NP   : constant Types.Pose := +Compose (+Bot, +P);
               begin
                  if Scan (I).D <= This.Max_Range then
                     Drawer.Draw
                       (D,
                        (Drawer_Point.Create
                           (Palette.Get_Gc (Pal'Unrestricted_Access, "blue"),
                            +NP.X, +NP.Y)));
                  end if;
               end;
            end loop;
         end;
      end loop;
      Drawer.Draw_End (D);
   end Draw;

   ----------------
   -- Draw_Laser --
   ----------------

   procedure Draw_Laser (This      : in Types.Posed_Range_Scan_Vectors.Vector;
                         Max_Range : in Types.Real) is

      Area  : Agpl.Gdk.Modal_Drawing_Area.Object;
      Laser : constant Draw_Laser_Data := (This, Max_Range);

   begin
      Area.Show (Laser, "Laser");
   end Draw_Laser;

end Sancta.Draw;
