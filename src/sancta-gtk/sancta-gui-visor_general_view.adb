with Agpl.If_Function;

with Sancta.Agent_Proxy;
--  with Sancta.Cost_Proxy;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Distributed;
with Sancta.Distributed.Datastore;
--  with Sancta.Distributed.Types;
with Sancta.Gui.Visor_Factory; pragma Elaborate_All (Sancta.Gui.Visor_Factory);
with Sancta.Gui.Visor_Widget.Handlers_U;
with Sancta.Gui.Visor_Widget.Handlers_UR;
--  with Sancta.Methods.Choose_Entry_Point;
--  with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Mission_Tracker;
with Sancta.Network.Messages;
--  with Sancta.Plugin.Annealer;
--  with Sancta.Plugin.Shared_Database;
--  with Sancta.Setpose_Xml;
with Sancta.Tasks.Explore_Edge;
with Sancta.Tasks.Explore_Directed_Edge;
with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Positioned;
with Sancta.Tasks.Speed_Driving;
with Sancta.Tasks.Wait_For_Orders;
with Sancta.Types.Operations;
--  with Sancta.Types.Real_Math;
with Sancta.Types.Transformations;

with Agpl.Conversions; use Agpl.Conversions;
with Sancta;
--  with Sancta.Assignment;
with Agpl.Gdk.Drawer_Arc;
with Agpl.Gdk.Drawer_Figures;
--  with Agpl.Gdk.Drawer_Point;
with Agpl.Gdk.Pango_Layout;
with Sancta.Plan_Node;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Utils;
--  with Agpl.If_Function;
--  with Agpl.Streams;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
--  with Agpl.Xml;
with Agpl; use Agpl;

with Dialog_Set_Criterion_Pkg;
with Dialog_Set_Pose_Pkg;

with Mbicp;

with Gdk.Dnd;      use Gdk.Dnd;
with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Gc;       use Gdk.Gc;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.Types;    use Gdk.Types;
with Gdk.Types.Keysyms;
with Gdk.Window;   use Gdk.Window;
with Gtk.Dnd;      use Gtk.Dnd;
with Gtk.Gentry;   use Gtk.Gentry;
with Gtk.Selection; use Gtk.Selection;

with Ada.Containers.Vectors;
with Ada.Text_Io; use Ada.Text_Io;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Sancta.Gui.Visor_General_View is

--     package Xml renames Agpl.Xml;

   use Sancta.Types;
   use type Sancta.Costs;
   use type Visor_Data.Object_Access;
   use type Visor_Data.Robot_Access;

   Robot_Size  : constant Float := 0.5;
   Zoom_Factor : constant Float := 1.25;

   function Iif is new Agpl.If_Function (String);

   Robot_Names : constant array (1 .. 4) of Node_Id :=
                   (Value ("ari"),
                    Value ("ben"),
                    Value ("ced"),
                    Value ("dan"));

   -----------------
   -- Adjust_Zoom --
   -----------------

   procedure Adjust_Zoom (This : in out Object) is
      Min_X, Min_Y : Float := Float'Last;
      Max_X, Max_Y : Float := Float'First;
      use Types.Transformations; use Real_Transf;

      --  Receives
      procedure Include (P : in Types.Pose) is
      begin
         Min_X := Float'Min (Min_X, +P.X);
         Min_Y := Float'Min (Min_Y, +P.Y);
         Max_X := Float'Max (Max_X, +P.X);
         Max_Y := Float'Max (Max_Y, +P.Y);
      end Include;
      Mode : Zoom_Modes := This.Drawer_Zoom_Mode;
   begin
      if Mode = Follow and then This.Selected_Bot = null then
         Mode := Fixed;
      elsif Mode = Mean and then This.Data.Get_Robots'Length = 0 then
         Mode := Fixed;
      end if;

      case Mode is
         when Follow =>
            --  Let's place the free space ahead of the robot!
            Include (+Compose (+This.Selected_Bot.Agent.Get_Pose,
              + (+This.Drawer_Zoom.Value, 0.0, 0.0)));
            Include (+Compose (+This.Selected_Bot.Agent.Get_Pose,
              + (-Types.Real (This.Drawer_Zoom.Value) / 4.0, 0.0, 0.0)));
            --           Include (+Compose (+This.Selected_Bot.Agent.Get_Pose,
            --                              + (0.0, - Types.Real (This.Drawer_Zoom), 0.0)));
            --           Include (+Compose (+This.Selected_Bot.Agent.Get_Pose,
            --                              + (0.0, + This.Drawer_Zoom, 0.0)));
            --           Include (+Compose (+This.Selected_Bot.Agent.Get_Pose,
            --                              + (-0.5, 0.0, 0.0)));

            This.Drawer.Set_Range_X (Min_X, Max_X);
            This.Drawer.Set_Range_Y (Min_Y, Max_Y);
         when Auto =>
            This.Drawer.Set_Range_X_Auto;
            This.Drawer.Set_Range_Y_Auto;
         when Fixed =>
            This.Drawer.Set_Range_X (This.Drawer_Zoom_X - This.Drawer_Zoom.Value,
                                     This.Drawer_Zoom_X + This.Drawer_Zoom.Value);
            This.Drawer.Set_Range_Y (This.Drawer_Zoom_Y - This.Drawer_Zoom.Value,
                                     This.Drawer_Zoom_Y + This.Drawer_Zoom.Value);
         when Mean =>
            This.Drawer_Zoom_X := 0.0;
            This.Drawer_Zoom_Y := 0.0;
            declare
               Bots : constant Visor_Data.Robot_Array := This.Data.Get_Robots;
            begin
               for I in Bots'Range loop
                  This.Drawer_Zoom_X :=
                    This.Drawer_Zoom_X + Float (Bots (I).Agent.Get_Pose.X);
                  This.Drawer_Zoom_Y :=
                    This.Drawer_Zoom_Y + Float (Bots (I).Agent.Get_Pose.Y);
               end loop;
               This.Drawer_Zoom_X := This.Drawer_Zoom_X / Float (Bots'Length);
               This.Drawer_Zoom_Y := This.Drawer_Zoom_Y / Float (Bots'Length);
            end;

            This.Drawer.Set_Range_X (This.Drawer_Zoom_X - This.Drawer_Zoom.Value,
                                     This.Drawer_Zoom_X + This.Drawer_Zoom.Value);
            This.Drawer.Set_Range_Y (This.Drawer_Zoom_Y - This.Drawer_Zoom.Value,
                                     This.Drawer_Zoom_Y + This.Drawer_Zoom.Value);
      end case;
   end Adjust_Zoom;

   ------------
   -- Create --
   ------------

   function Create (Context : Visor_Factory.Creation_Context)
                    return    Visor_Widget.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      Gtk_New (This.Area);

      Set_Flags (This.Area, Can_Focus);
      --  To allow keyboard interaction

      This.Draw_Grid := Context.Options.Draw_Grid;

      Put_Line ("Connecting...");

      --  Connect events
      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "button_press_event",
         Visor_Widget.Handlers_Ur.To_Marshaller (Clicked'Access),
         Visor_Widget.Object_Access (This));

      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "expose-event",
         Visor_Widget.Handlers_Ur.To_Marshaller (Expose'Access),
         Visor_Widget.Object_Access (This));

      Visor_Widget.Handlers_U.Connect
        (This.Area,
         "drag_end",
         Robot_Drag_End'Access,
         Visor_Widget.Object_Access (This));
      --  IIUC, I should use drag_drop, but for some reason I don't manage to
      --  get this signal propagated, so I won't lose more time over this,
      --  even if this is a hack.

      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "drag_motion",
         Robot_Drag_Happening'Access,
         Visor_Widget.Object_Access (This));

      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "key_press_event",
         Visor_Widget.Handlers_Ur.To_Marshaller (Key_Press'Access),
         Visor_Widget.Object_Access (This));

      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "key_release_event",
         Visor_Widget.Handlers_Ur.To_Marshaller (Key_Release'Access),
         Visor_Widget.Object_Access (This));

      Put_Line ("Connected!");

      return Visor_Widget.Object_Access (This);
   exception
      when E : others =>
         Put_Line ("General_View.Create: " & Report (E));
         Log ("General_View.Create: " & Report (E), Error);
         return null;
   end Create;

   procedure Dialog_Set_Criterion (This : in out Object) is
      use Dialog_Set_Criterion_Pkg;
      Dialog : Dialog_Set_Criterion_Access;
   begin
      Gtk_New (Dialog);
      Show_All (Dialog);

      Set_Text (Dialog.Minmax, "1.0");
      Set_Text (Dialog.Minsum, "1.0");

      Visor_Widget.Handlers_U.Connect
        (Dialog.Button_Ok,
         "clicked",
         Set_Criterion_Ok'Access,
         This'Unchecked_Access);
 end Dialog_Set_Criterion;

   ---------------------
   -- Dialog_Set_Pose --
   ---------------------

   procedure Dialog_Set_Pose (This : in out Object) is
      use Dialog_Set_Pose_Pkg;
      Dialog : Dialog_Set_Pose_Access;
   begin
      if This.Selected_Bot = null then
         return;
      end if;

      Gtk_New (Dialog);
      Show_All (Dialog);

      Set_Text (Dialog.Entry_X,
                To_String (Float (This.Selected_Bot.Agent.Get_Pose.X)));
      Set_Text (Dialog.Entry_Y,
                To_String (Float (This.Selected_Bot.Agent.Get_Pose.Y)));
      Set_Text (Dialog.Entry_A,
                To_String (Float (This.Selected_Bot.Agent.Get_Pose.A), 5));

      Visor_Widget.Handlers_U.Connect
        (Dialog.Button_Ok,
         "clicked",
         Set_Pose_Ok'Access,
         This'Unchecked_Access);
   end Dialog_Set_Pose;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    This   :        Visor_Widget.Object_Access)
                    return          Boolean
   is
      pragma unreferenced (Widget, Event);
      use Agpl.Gdk.Palette;
      use Agpl.Gdk.Drawer;
      use Agpl.Gdk.Drawer_Figures;
      X : constant Visor_General_View.Object_Access :=
            Visor_General_View.Object_Access (This);
      W : constant Gdk_Drawable := Get_Window (X.Area);
      D : Agpl.Gdk.Drawer.Object renames X.Drawer;

      Black : constant String := "#000000";
      Gray  : constant String := "#0f0f0f";
      White : constant String := "#ffffff";

      Worst_Cost : Sancta.Costs := 0.0;
      Total_Cost : Sancta.Costs := 0.0;

      -------------------------
      -- Draw_Selected_Robot --
      -------------------------

      procedure Draw_Selected_Robot is
      begin
         --  Double Draw selected robot
         if X.Selected_Bot /= null and then
            X.Selected_Bot.Last_Seen.Elapsed < 5.0
         then
            declare
               Bot : Visor_Data.Robot_Data renames X.Selected_Bot.all;
            begin
               if X.Draw_Sight then
                  declare
                     use Types.Transformations; use Real_Transf;
                     Sight : constant Types.Pose :=
                               +Compose (+Bot.Agent.Get_Pose,
                                         + (+X.Drawer_Zoom.Value * 0.9, 0.0, 0.0));
                  begin
                     Draw_Arrow (+Bot.Agent.Get_Pose.X,
                                 +Bot.Agent.Get_Pose.Y,
                                 +Sight.X, +Sight.Y,
                                 1.0,
                                 Get_Color (X.Pal'Access, +Bot.Gui_Color), D);
                  end;
               end if;

               case X.Interaction is
               when Drag_Robots =>
                  if X.Dragging then
                     --  Draw at the dragging pose
                     Draw_Robot (+X.Drag_Pose.X,
                                 +X.Drag_Pose.Y,
                                 Float (X.Drag_Pose.A),
                                 Get_Color (X.Pal'Access, +Bot.Gui_Color), D,
                                 Robot_Size * 1.2);
                  else
                     --  Draw at its current pose
                     Draw_Robot (+Bot.Agent.Get_Pose.X,
                                 +Bot.Agent.Get_Pose.Y,
                                 Float (Bot.Agent.Get_Pose.A),
                                 Get_Color (X.Pal'Access, +Bot.Gui_Color), D,
                                 Robot_Size * 1.2);
                  end if;
               when others =>
                  --  Draw at its current pose
                  Draw_Robot (+Bot.Agent.Get_Pose.X,
                              +Bot.Agent.Get_Pose.Y,
                              Float (Bot.Agent.Get_Pose.A),
                              Get_Color (X.Pal'Access, +Bot.Gui_Color), D,
                              Robot_Size * 1.2);
               end case;
            end;
         end if;
      end Draw_Selected_Robot;

   begin
      if not X.Dnd_Ready then
         Prepare_Dnd (X.all);
      end if;

      Set_Drawable (X.Pal, W);
      D.Set_Drawable (W);
      D.Set_Widget (Gtk_Widget (X.Area));

      Adjust_Zoom (X.all);

      Draw_Begin (D);

      Set_Background (W, Get_Color (X.Pal'Access, White));
      Clear (W);

      if X.Draw_Grid then
         --  Draw origin:
         Draw_Plus (0.0, 0.0, Get_Color (X.Pal'Access, Gray), D);
         --  Draw some grid for scale awareness:
         Draw_Grid_Points (-5.0, -5.0, 5.0,  5.0,
                           Get_Color (X.Pal'Access, Gray), D);
      end if;

      --  Draw things obtained elsewhere:
      Visor_Data.Drawer (X.Data).Flush (D);

      --  Draw robots
      declare
         Bots : constant Visor_Data.Robot_Array :=
                  Visor_Data.Get_Robots (X.Data.all);
         Text_Row   : Gint := 30;
      begin
         for I in Bots'Range loop
            declare
               Bot : Visor_Data.Robot_Data renames Bots (I).all;

               -- Draw_Scans --

               procedure Draw_Scans is
                  use Types.Operations;
                  use Types.Operations.Real_Transf;
                  type Color_Array is
                    array (1 ..
                             Natural (Bot.Laser_Scans.Length)) of String (1 .. 7);
                  function Prepare_Gradient return Color_Array is
                     Color_Gradient : constant Integer :=
                                        64 / Integer'Max
                                          (Integer (Bot.Laser_Scans.Length), 1);
                     Result         : Color_Array;
                  begin
                     for I in Result'Range loop
                        if I = Result'Last then
                           Result (I) := "#a0a0ff";
                        else
                           Result (I) := Agpl.Gdk.Palette.Get_Color_Name
                             (Sancta.Types.Rgb_Component
                                (192 + Color_Gradient * (Bot.Laser_Scans.Last_Index - I)),
                              Sancta.Types.Rgb_Component
                                (192 + Color_Gradient * (Bot.Laser_Scans.Last_Index - I)),
                              255);
                        end if;
                     end loop;
                     return Result;
                  end Prepare_Gradient;
                  Color : constant Color_Array := Prepare_Gradient;
               begin
                  --  Log ("Drawing scans" & Bot.Laser_Scans.Length'Img, Always);
                  for I in Bot.Laser_Scans.First_Index .. Bot.Laser_Scans.Last_Index loop
                     declare
                        Scan     : constant Network.Messages.Laser_Type :=
                                  Bot.Laser_Scans.Element (I);
                        Laser_Gc_Thick : constant Gdk_Gc :=
                                           Get_Gc (X.Pal'Unchecked_Access,
                                                   Color (I),
                                                   Line_Width => 3);
                        Laser_Gc_Thin  : constant Gdk_Gc :=
                                           Get_Gc (X.Pal'Unchecked_Access,
                                                   Color (I),
                                                   Line_Width => 1);
                        Laser_Center   : constant Types.Pose :=
                                           +Compose (+Scan.Robot_Pose,
                                                     +Scan.Laser_Pose);
                        Prev_P         : Types.Pose := Laser_Center;
                        --  Initialized just to avoid a warning

                        T              : constant Transformation :=
                                           Get_Composition (+Scan.Robot_Pose) *
                                           Get_Composition (+Scan.Laser_Pose);
                     begin
                        for J in Scan.Range_Scan'First ..
                                 Scan.Range_Scan'Last
                        loop
                           declare
                              use Types.Transformations;
                              PP : constant Types.Pose := Polar_To_Cart
                                (Scan.Range_Scan (J).A,
                                 Types.Real'Min (Scan.Range_Scan (J).D,
                                                 X.Out_Of_Range));
                              P : constant Types.Pose :=
                                    + (T * (+PP.X, +PP.Y, 0.0, 1.0));
                           begin
                              case X.Laser_Draw is
                                 when Points =>
                                    --  Use a line since points don't seem to honor
                                    --  the thickness of the GC
                                    Agpl.Gdk.Drawer_Figures.Draw_Segment
                                      (+P.X, + P.Y,
                                       +P.X, + P.Y,
                                       Laser_Gc_Thick, D);
                                 when Segments =>
                                    Agpl.Gdk.Drawer_Figures.Draw_Segment
                                      (+Prev_P.X, +Prev_P.Y,
                                       +P.X, +P.Y,
                                       Laser_Gc_Thin, D);
                                    Prev_P := P;
                              end case;
                           end;
                        end loop;
                        if X.Laser_Draw = Segments then -- Draw the closing line:
                           Agpl.Gdk.Drawer_Figures.Draw_Segment
                             (+Prev_P.X, +Prev_P.Y,
                              +Laser_Center.X, +Laser_Center.Y,
                              Laser_Gc_Thin, D);
                        end if;
                     end;
                  end loop;
               end Draw_Scans;

            begin
               if Bot.Last_Seen.Elapsed < 5.0 then
                  --  Textual info about cost:
--                    declare
--                       Plan_Cost   : constant Sancta.Costs := Bot.Cost;
--                       Cost_Layout :          Agpl.Gdk.Pango_Layout.Object
--                         (Create_Pango_Layout
--                            (X.Area,
--                             Bot.Agent.Get_Name & " cost:"
--                             & To_String (Plan_Cost)));
--                    begin
--                       Draw_Layout (W, Get_Color (X.Pal'Unchecked_Access,
--                                                  +Bot.Gui_Color),
--                                    10, Text_Row, Cost_Layout.Layout);
--                       Text_Row := Text_Row + 10;
--                       if Plan_Cost = Sancta.Infinite or else
--                         Sancta.Infinite - Total_Cost < Plan_Cost
--                       then
--                          Total_Cost := Sancta.Infinite;
--                       else
--                          Total_Cost := Total_Cost + Plan_Cost;
--                       end if;
--                       Worst_Cost := Sancta.Costs'Max (Worst_Cost, Plan_Cost);
--                    end;

                  --  Put a red * if recording at origin
                  if X.Data.Is_Recording then
                     declare
                        Rec_Layout : Agpl.Gdk.Pango_Layout.Object
                          (Create_Pango_Layout (X.Area, "*"));
                     begin
                        Draw_Layout (W, Get_Color (X.Pal, "red"),
                                     1, 1, Rec_Layout.Layout);
                     end;
                  end if;

                  --  Textual info for the robot:

                  declare
                     function Iif is new If_Function (String);
                     Name_Layout : Agpl.Gdk.Pango_Layout.Object
                       (Create_Pango_Layout
                          (X.Area,
                           Iif (X.Selected_Bot = Bots (I), "* ", "") &
                           Agent_Proxy.Get_Name (Bot.Agent) & " " &
                           Iif
                             (X.Data.Config.Show_Poses or X.Draw_Pose,
                              To_String (Agent_Proxy.Get_Pose (Bot.Agent)),
                              "")));
                     Pos         : constant Agpl.Gdk.Float_Vector :=
                                     X.Drawer.Transform
                                       ((+Bot.Agent.Get_Pose.X + 0.5,
                                         +Bot.Agent.Get_Pose.Y - 0.5,
                                         1.0));
                  begin
                     Draw_Layout (W, Get_Color (X.Pal'Unchecked_Access,
                                                +Bot.Gui_Color),
                                  Gint (Pos (1)), Gint (Pos (2)),
                                  Name_Layout.Layout);
                  end;

                  --  Draw laser scans
                  Draw_Scans;

                  --  Log ("Drawing robot " & S (Bot.Name), Always);
                  Draw_Robot (+Bot.Agent.Get_Pose.X,
                              +Bot.Agent.Get_Pose.Y,
                              Float (Bot.Agent.Get_Pose.A),
                              Get_Color (X.Pal'Access, +Bot.Gui_Color), D,
                              Robot_Size);

                  --  Draw tasks
                  --  We use Prev_Pose to store a meaningful previous pose.
                  --  We use Use_Pose to signal if Prev_Pose is valid
                  declare
                     use Sancta.Tasks.Containers.Lists;
                     Tasks     : constant List := Bot.Agent.Get_Tasks;
                     Prev_Pose : Types.Pose;
                     Use_Pose  : Boolean;
                     I         : Cursor;
                  begin
                     I := Tasks.First;

                     --  Prepare initial variables depending on first task class
                     if Has_Element (I) then
                        if Element (I) in Sancta.Tasks.Positioned.Object'Class or else
                          Element (I) in Sancta.Tasks.Explore_Directed_Segment.Object'Class or else
                          Element (I) in Sancta.Tasks.Explore_Directed_Edge.Object'Class
                        then
                           Prev_Pose := Bot.Agent.Get_Pose;
                           Use_Pose  := True;
                        end if;
                     end if;

                     while Has_Element (I) loop
                        if Element (I) in Sancta.Tasks.Positioned.Object'Class then
                           declare
                              New_Pose : Types.Pose renames
                                Sancta.Tasks.Positioned.Object (Element (I)).Pose;
                              --  Goal_Size : constant Float := 0.2;
                           begin
                              --  Joining path
                              if Use_Pose then
                                 Agpl.Gdk.Drawer_Figures.Draw_Segment
                                   (+Prev_Pose.X, +Prev_Pose.Y, +New_Pose.X, +New_Pose.Y,
                                    Get_Gc (X.Pal'Unchecked_Access,
                                            +Bot.Gui_Color,
                                            Line_Style => Line_On_Off_Dash), D);
                              end if;
                              Prev_Pose := New_Pose;
                              Use_Pose  := True;

                              --  Bot at goal
--                                Draw_Robot (+New_Pose.X,
--                                            +New_Pose.Y,
--                                            Float (New_Pose.A),
--                                            Get_Color (X.Pal'Access, +Bot.Gui_Color), D);
                              D.Draw
                                (Agpl.Gdk.Drawer_Arc.Create_Circle
                                   (Get_Color (X.Pal'Access, +Bot.Gui_Color),
                                    +New_Pose.X,
                                    +New_Pose.Y, 0.5));
                           end;
                        elsif Element (I) in Sancta.Tasks.Explore_Directed_Segment.Object'Class then
                           declare
                              Segment   : Sancta.Tasks.Explore_Directed_Segment.Object renames
                                Sancta.Tasks.Explore_Directed_Segment.Object (Element (I));
                              Join_Pose : Types.Pose;
                           begin
                              --  Straight Path (arrow):
                              Agpl.Gdk.Drawer_Figures.Draw_Arrow
                                (+Segment.Get_From.X, +Segment.Get_From.Y,
                                 +Segment.Get_To.X, +Segment.Get_To.Y,
                                 0.5,
                                 Get_Gc (X.Pal'Unchecked_Access,
                                         +Bot.Gui_Color,
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
                                    Get_Gc (X.Pal'Unchecked_Access,
                                            +Bot.Gui_Color,
                                            Line_Style => Line_On_Off_Dash), D);
                              end if;

                              Prev_Pose := Segment.Get_To;
                              Use_Pose  := True;
                           end;
                        elsif Element (I) in Sancta.Tasks.Explore_Directed_Edge.Object'Class then
                           declare
                              Segment   : Sancta.Tasks.Explore_Directed_Edge.Object renames
                                Sancta.Tasks.Explore_Directed_Edge.Object (Element (I));
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
                                 Get_Gc (X.Pal'Unchecked_Access,
                                         +Bot.Gui_Color,
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
                                    Get_Gc (X.Pal'Unchecked_Access,
                                            +Bot.Gui_Color,
                                            Line_Style => Line_On_Off_Dash), D);
                              end if;

                              Prev_Pose := Pose_Fin;
                              Use_Pose  := True;
                           end;
                        else
                           Use_Pose := False;
                           --                          Log ("Undrawable task: " &
                           --                               External_Tag (Element (I)'Tag),
                           --                               Warning);
                        end if;

                        Next (I);
                     end loop;
                  end;
               end if; --  Last seen
            end;
         end loop;
      end;

      --  Double Draw selected robot
      Draw_Selected_Robot;

      --  Textual info about plan costs:
--        declare
--           Cost_Layout : Agpl.Gdk.Pango_Layout.Object
--             (Create_Pango_Layout
--                (X.Area,
--                 "Total cost:" & To_String (Total_Cost)));
--        begin
--           Draw_Layout (W, Get_Color (X.Pal'Unchecked_Access, Black),
--                        10, 10, Cost_Layout.Layout);
--        end;
--        declare
--           Cost_Layout : Agpl.Gdk.Pango_Layout.Object
--             (Create_Pango_Layout
--                (X.Area,
--                 "Minimax cost:" & To_String (Worst_Cost)));
--        begin
--           Draw_Layout (W, Get_Color (X.Pal'Unchecked_Access, Black),
--                        10, 20, Cost_Layout.Layout);
--        end;

      --  Textual info about user mode:
      declare
         Mode_Layout : Agpl.Gdk.Pango_Layout.Object
           (Create_Pango_Layout
              (X.Area,
               "Click mode: " & X.Interaction'Img &
               "; View mode: " & X.Drawer_Zoom_Mode'Img));
         Height, Width : Gint;
      begin
         Get_Size (W, Height => Height, Width => Width);
         Draw_Layout (W, Get_Color (X.Pal'Unchecked_Access, Black),
                      10, Height - 15, Mode_Layout.Layout);
      end;

      Draw_End (D);

      return False;
   exception
      when E : others =>
         Put_Line ("Visor_General_View.Expose: " & Report (E));
         Log ("Visor_General_View.Expose: " & Report (E), Error);
         return False;
   end Expose;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : in Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Area);
   end Get_Widget;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object)
   is
   begin
      This.Data := Visor_Data.Object_Access (Data);

      --  Other initialization:
      This.Out_Of_Range := This.Data.Config.Laser_Range;
   end Set_Data;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Object;
                     Id   : in     Node_Id;
                     Msg  : in     Robot_Data.Network_Update)
   is
      pragma Unreferenced (Id);
      use Gui.Robot_Data;
   begin
      case Msg.Kind is
         when Current_Task | All_Tasks =>
            Redraw (This);
         when Gui.Robot_Data.Pose =>
            Redraw (This);
         when others =>
            null;
      end case;
   end Update;

   -----------------
   -- Prepare_Dnd --
   -----------------

   procedure Prepare_Dnd (This : in out Object) is
      --  We're leaking the string used here, but is a very small leak
      --  so we leave it (lazy me...)
      Empty_Cursor : constant Gdk_Pixbuf := Gdk_New (Width => 1, Height => 1);
   begin
      This.Dnd_Ready := True;

      Source_Set (This.Area,
                  Button1_Mask or Button3_Mask,
                  (1 => (Target => New_String (Drag_N_Drop_Pose),
                         Flags  => Target_Same_Widget,
                         Info   => Drag_N_Drop_Pose_Flag)),
                  Action_Private);

      Source_Set_Icon_Pixbuf (This.Area, Empty_Cursor);

      Dest_Set (This.Area,
                Targets => (1 => (Target => New_String (Drag_N_Drop_Pose),
                                  Flags  => Target_Same_Widget,
                                  Info   => Drag_N_Drop_Pose_Flag)));
   end Prepare_Dnd;

   -------------
   -- Clicked --
   -------------

   function Clicked (Widget : access Gtk_Widget_Record'Class;
                     Event  :        Gdk.Event.Gdk_Event_Button;
                     This   :        Visor_Widget.Object_Access)
                     return   Boolean
   is
      pragma Unreferenced (Widget);
   begin
      declare
         Thix    : constant Visor_General_View.Object_Access :=
                     Visor_General_View.Object_Access (This);
         --  Keep coordinates and button just in case that drags begins later:
         X       : constant Gint := Gint (Get_X (Event));
         Y       : constant Gint := Gint (Get_Y (Event));
         P       : constant Agpl.Gdk.Float_Vector :=
                     Thix.Drawer.Transform_Back ((Float (X), Float (Y), 1.0));
         Pose    : constant Types.Pose := (+P (1), +P (2), 0.0);
      begin
         This.Get_Widget.Grab_Focus;
         case Thix.Interaction is
         when Go_To_Place =>
            if Thix.Selected_Bot /= null then
               Log ("Sending " & Thix.Selected_Bot.Agent.Get_Name &
                    " to " & To_String (Pose), Always);
               Visor_Data.Get_Link (Thix.Data).Send
                 (Network.New_Address
                    (Thix.Selected_Bot.Agent.Get_Id,
                     Gui_Channel),
                  Network.Messages.Set_Task
                    (Tasks.Goto_Pose.Create (Pose      => Pose,
                                             Use_Angle => False)));
               --  Thix.Interaction := Drag_Robots;
            end if;

         when Auction_Goal =>
            Log ("Auctioning " & To_String (Pose), Always);
            Thix.Data.Add_Task_For_Auction
              (Tasks.Goto_Pose.Create (Pose      => Pose,
                                       Use_Angle => False));
            --              declare
            --                 Ok : Boolean;
            --              begin
            --                 Plugin.Annealer.Add_Task
            --                   (Tasks.Goto_Pose.Create (Pose      => Pose,
            --                                            Use_Angle => False), Ok);
            --              end;

         when Drag_Robots | Guide_Robot =>
            if not Thix.Dragging then
               Thix.Selected_Bot := Visor_Data.Locate_Robot (Thix.Data,
                                                             Pose);
               if Thix.Selected_Bot /= null then
                  Thix.Drag_Pose := Thix.Selected_Bot.Agent.Get_Pose;
                  Thix.Drag_X    := X;
                  Thix.Drag_A    := Thix.Drag_Pose.A;
               else
                  return False;
               end if;

               case Get_Button (Event) is
                  when 1      => Thix.Drag_Kind := Xy;
                  when 3      => Thix.Drag_Kind := Ang;
                  when others => Thix.Drag_Kind := Invalid;
               end case;
            end if;
         end case;

         return False;
      end;
   exception
      when E : others =>
         Log ("At clicked: " & Report (E), Error, Log_Section);
         return False;
   end Clicked;

   -------------------
   -- Correct_Angle --
   -------------------

   procedure Correct_Angle (This : in out Object;
                            Bot  : in     Visor_Data.Robot_Access) is
      use Types.Operations;
   begin
      if Bot = null then
         return;
      end if;

      declare
         Old_Pose : constant Types.Pose := Bot.Agent.Get_Pose;
         New_Pose : constant Types.Pose :=
                      Corrected_Pose
                        (Old_Pose,
                         Angle_From_Scan
                           (Bot.Laser_Scans.Last_Element.Range_Scan,
                            Out_Of_Range => 31.0),
                         Tolerance => 0.5);
      begin
         if Old_Pose = New_Pose then
            Log ("No angle correction found", Warning);
         else
            Send_Message (This,
                          Bot,
                          Network.Messages.Set_Pose (New_Pose));
         end if;
      end;
   end Correct_Angle;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press (Widget : access Gtk_Widget_Record'Class;
                       Event  :        Gdk.Event.Gdk_Event_Key;
                       This   :        Visor_Widget.Object_Access)
                       return Boolean
   is
      Thix    : constant Visor_General_View.Object_Access :=
                  Visor_General_View.Object_Access (This);
      use Gdk.Types.Keysyms;
   begin
      case Get_Key_Val (Event) is
         when GDK_LC_Q =>
            Thix.Interaction := Drag_Robots;
         when GDK_LC_W =>
            Thix.Interaction := Go_To_Place;
         when GDK_LC_E =>
            Thix.Interaction := Guide_Robot;
         when GDK_LC_R =>
            Thix.Interaction := Auction_Goal;

         when Gdk_Lc_T =>
            null;
            --  Plugin.Annealer.Toggle;
         when Gdk_Lc_Y =>
            Dialog_Set_Criterion (Thix.all);

         when GDK_LC_L =>
            Match_Two_Bots (Thix.all, Thix.Former_Bot, Thix.Selected_Bot);

         --  Halting and resuming of whole team:
         when GDK_LC_H =>
            --  Still the robots
            declare
               Wait : Tasks.Wait_For_Orders.Object;
               Bots : constant Visor_Data.Robot_Array := Thix.Data.Get_Robots;
            begin
               for I in Bots'Range loop
                  Prepend_Task (Thix.all, Bots (I), Wait);
               end loop;
            end;
         when GDK_LC_J =>
            declare
               Bots : constant Visor_Data.Robot_Array := Thix.Data.Get_Robots;
            begin
               for I in Bots'Range loop
                  Remove_First_Task (Thix.all, Bots (I));
               end loop;
            end;

         --  Mission related
         when GDK_LC_M =>
            Mission_Start (Thix.all);
         when GDK_LC_N =>
            Mission_Acquire (Thix.all);

         --  Rotate the believed pose:
         when GDK_LC_Z =>
            Visor_Data.Get_Link (Thix.Data).Send
              (Network.New_Address
                 (Thix.Selected_Bot.Agent.Get_Id,
                  Gui_Channel),
               Network.Messages.Set_Pose
                 (Thix.Selected_Bot.Agent.Get_Pose - (0.0, 0.0, 0.05)));
         when GDK_LC_X =>
            --  Rotate the believed pose:
            Visor_Data.Get_Link (Thix.Data).Send
              (Network.New_Address (Thix.Selected_Bot.Agent.Get_Id,
               Gui_Channel),
               Network.Messages.Set_Pose
                 (Thix.Selected_Bot.Agent.Get_Pose + (0.0, 0.0, 0.05)));
         when GDK_LC_C =>
            --  Auto-adjust angle
            Correct_Angle (Thix.all,
                           Thix.Selected_Bot);
         when Gdk_Lc_V =>
            --  Show dialog for pose adjusting
            Dialog_Set_Pose (Thix.all);

         when Gdk_Lc_B =>
            --  Negate Sight drawing
            Thix.Draw_Sight := not Thix.Draw_Sight;

         when GDK_LC_G =>
            --  Draw Grid
            Thix.Draw_Grid := not Thix.Draw_Grid;

         when GDK_LC_P =>
            --  Draw poses
            Thix.Draw_Pose := not Thix.Draw_Pose;

         when GDK_0 =>
            Thix.Selected_Bot := null;
         when GDK_1 .. GDK_4 =>
            --  Choose bot
            declare
               New_Selected : constant Visor_Data.Robot_Access :=
                                Visor_Data.Locate_Robot
                                  (Thix.Data,
                                   Image
                                     (Robot_Names
                                        (Integer
                                           (Get_Key_Val (Event) - Gdk_0))));
            begin
               if Thix.Selected_Bot /= New_Selected then
                  Thix.Former_Bot   := Thix.Selected_Bot;
                  Thix.Selected_Bot := New_Selected;
               end if;
            end;
         when Gdk_F9 .. Gdk_F12 =>
            --  Send mission
            declare
               M    : Mission_Tracker.Object;
               Name : constant String :=
                        "mission" &
               Trim (Integer (Get_Key_Val (Event) - Gdk_F8)'Img) &
               ".xml";
            begin
               Log ("Sending mission " & Name, Informative);
               M.Parse_File (Name);
               M.Print_Summary;
               Thix.Mission_Start (M.Get_Plan);
            end;
         when Gdk_Lc_A =>
            --  Still the robot
            declare
               Wait : Tasks.Wait_For_Orders.Object;
            begin
               Prepend_Task (Thix.all, Thix.Selected_Bot, Wait);
            end;
         when GDK_LC_S =>
            --  Remove robot first task
            Remove_First_Task (Thix.all, Thix.Selected_Bot);
         when GDK_Space =>
            --  Clear bot tasks (stop it).
            if Thix.Selected_Bot /= null then
               Visor_Data.Get_Link (Thix.Data).Send
                 (Network.New_Address (Thix.Selected_Bot.Agent.Get_Id,
                  Gui_Channel),
                  Network.Messages.Clear_Tasks);
            end if;
         when GDK_Up | GDK_Left | GDK_Down | GDK_Right =>
            --  Manual driving
            if Thix.Selected_Bot /= null then
               declare
                  During     : constant array (Boolean) of Duration :=
                                 (False => 0.5, True => 2.5);
                  Velocity   :          Types.Pose;
               begin
                  case Get_Key_Val (Event) is
                     when GDK_Up    => Velocity := ( 1.5, 0.0,  0.0);
                     when GDK_Down  => Velocity := (-1.5, 0.0,  0.0);
                     when GDK_Left  => Velocity := ( 0.0, 0.0,  0.4);
                     when GDK_Right => Velocity := ( 0.0, 0.0, -0.4);
                     when others    => null;
                  end case;
                  Prepend_Task (Thix.all,
                                Thix.Selected_Bot,
                                Tasks.Speed_Driving.Create
                                  (Velocity,
                                   During (Thix.Caps_On)));
               end;
            end if;
         when GDK_Shift_L =>
            Thix.Caps_On := True;
         when Gdk_F5 .. Gdk_F8 =>
            Thix.Drawer_Zoom_Mode :=
              Zoom_Modes'Val (Get_Key_Val (Event) - Gdk_F5);
            Thix.Redraw;
         when GDK_KP_Add | GDK_Plus =>
            Thix.Drawer_Zoom.Set
              (Thix.Drawer_Zoom.Value / Zoom_Factor, Period => 1.0);
            if Thix.Drawer_Zoom_Mode = Auto then
               Thix.Drawer_Zoom_Mode := Mean;
            end if;
         when GDK_KP_Subtract | GDK_Minus =>
            Thix.Drawer_Zoom.Set
              (Thix.Drawer_Zoom.Value * Zoom_Factor, Period => 1.0);
            if Thix.Drawer_Zoom_Mode = Auto then
               Thix.Drawer_Zoom_Mode := Mean;
            end if;

--           when Gdk_Plus =>
--              Thix.Data.Set_Recording (not Thix.Data.Is_Recording);

         when Gdk_F1 =>
            Print_Help;
         when others =>
            Log ("Some key pressed: " & Get_Key_Val (Event)'Img, Trace.Debug,
                 Section => Detail_Section);
      end case;

      Queue_Draw (Widget);
      return False;
   exception
      when E : others =>
         Log ("Key_Pressed: " & Report (E), Warning);
         return False;
   end Key_Press;

   -----------------
   -- Key_Release --
   -----------------

   function Key_Release (Widget : access Gtk_Widget_Record'Class;
                         Event  :        Gdk.Event.Gdk_Event_Key;
                         This   :        Visor_Widget.Object_Access)
                         return Boolean
   is
      Thix    : constant Visor_General_View.Object_Access :=
                  Visor_General_View.Object_Access (This);
      use Gdk.Types.Keysyms;
   begin
      case Get_Key_Val (Event) is
         when GDK_Shift_L =>
            Thix.Caps_On := False;
         when others => null;
      end case;

      Queue_Draw (Widget);
      return False;
   end Key_Release;

   --------------------
   -- Match_Two_Bots --
   --------------------

   procedure Match_Two_Bots (This       : in Object;
                             Bot1, Bot2 : in Visor_Data.Robot_Access) is
      subtype N is Natural;
      package SM renames MBICP;
      subtype Sm_Float is SM.Mbicp_Float;
      use Types.Operations;
      use Types.Operations.Real_Transf;
      use type Sm_Float;

      function To_SM_Laser (L : in Types.Range_Scan) return SM.Laser_Scan is
         Result : SM.Laser_Scan (L'Range);
--         use Types.Real_Math;
      begin
         for I in Result'Range loop
            Result (I).Bearing  := SM.Mbicp_Float (L (I).A);
            Result (I).Distance := SM.Mbicp_Float (L (I).D);
         end loop;

         return Result;
      end To_SM_Laser;

      Min_Error : SM_Float := Sm_Float'Last;
      Max_Assoc : Natural := 0;

      type Matching is record
         Assocs : Natural;
         Error  : SM_Float;
         Result : Types.Pose; --  Delta matched in *robocentric* coordinates
                              --  Not *lasercentric* !!!!
      end record;

      package Match_Vectors is new Ada.Containers.Vectors (Positive, Matching);
      Matches : Match_Vectors.Vector;

      -----------------
      -- Match_Value --
      -----------------

      function Match_Value (This : in Matching) return Float is
      begin
         return Float (This.Assocs) / Float (This.Error * 100_000.0);
      end Match_Value;

      ---------------
      -- Try_Match --
      ---------------

      procedure Try_Match (Delta_Pose : in Types.Pose) is
         Result     : SM.Pose;
         Outcome    : SM.Outcomes;
      begin
         SM.Init (Bw => 0.5, Br => 1.0, Max_Iter => 100);
         SM.Set_Out_Of_Range (31.0);

         SM.Match (To_SM_Laser (Bot1.Laser_Scans.Element
                                  (Bot1.Laser_Scans.Last_Index).Range_Scan),
                   To_SM_Laser (Bot2.Laser_Scans.Element
                                  (Bot2.Laser_Scans.Last_Index).Range_Scan),
                   (SM.Mbicp_Float (Delta_Pose.X),
                    SM.Mbicp_Float (Delta_Pose.Y),
                    SM.Mbicp_Float (Delta_Pose.A)),
                   Result,
                   Outcome);

         if Outcome in Sm.Outcomes_Ok then
            declare
               Pose_Result : constant Types.Pose :=
                               (Real (Result.X),
                                Real (Result.Y),
                                To_Angle (Real (Result.A)));

               L1          : constant Network.Messages.Laser_Type :=
                               Bot1.Laser_Scans.Last_Element;
               L2          : constant Network.Messages.Laser_Type :=
                               Bot2.Laser_Scans.Last_Element;

               Match : constant Matching :=
                         (Assocs => SM.Get_Num_Filtered_Associations,
                          Error  => SM.Get_Mean_Error,
                          Result => +Compose (+L1.Laser_Pose,
                                              Compose (+Pose_Result,
                                                       Invert (+L2.Laser_Pose))));
            begin
               Matches.Append (Match);
               Max_Assoc := Natural'Max (Max_Assoc, Match.Assocs);
               Min_Error := SM_Float'Min (Min_Error, Match.Error);
               Log ("A:" & To_String (Match.Assocs) &
                    "; E:" & To_String (Float (Match.Error), 7) &
                    "; V:" & To_String (Float (Match_Value (Match)), 7) &
                    "; D:" & To_String (Delta_Pose) &
                    "; SM:" & To_String (Pose_Result) &
                    "; R:" & To_String (Match.Result),
                    Trace.Debug);
            exception
               when E : others =>
                  Log ("Try_Match: " & Report (E), Warning);
            end;
         else
            Log ("Not converged (" & Outcome'Img &
                 ") for delta " & To_String (Delta_Pose),
                 Trace.Debug);
         end if;
      end Try_Match;

      Delta_Pose : Types.Pose;

   begin
      if Bot1 = null or else Bot2 = null then
         Log ("Missing bot(s) for match!", Warning);
         return;
      elsif Bot1 = Bot2 then
         Log ("Can't match bot against himself!", Warning);
         return;
      elsif N (Bot1.Laser_Scans.Length) < 1 or else
            N (Bot2.Laser_Scans.Length) < 1
      then
         return;
      end if;

      declare
         L1 : constant Network.Messages.Laser_Type :=
                Bot1.Laser_Scans.Last_Element;
         L2 : constant Network.Messages.Laser_Type :=
                Bot2.Laser_Scans.Last_Element;
      begin
--           Log ("L1.R = " & To_String (L1.Robot_Pose), Always);
--           Log ("L1.L = " & To_String (L1.Laser_Pose), Always);
--           Log ("L2.R = " & To_String (L2.Robot_Pose), Always);
--           Log ("L1.L = " & To_String (L2.Laser_Pose), Always);
         Delta_Pose :=
           +Decompose (Compose (+L1.Robot_Pose,
                                +L1.Laser_Pose),
                       Compose (+L2.Robot_Pose,
                                +L2.Laser_Pose));
      end;

      Log ("Delta between laser centers is " & To_String (Delta_Pose),
           Trace.Debug, Section => Log_Section);

      declare
         BX : constant Real  := -1.5;
         BY : constant Real  := -1.5;
         BA : constant Angle := -0.5;
         DX : Real;
         DY : Real;
         DA : Angle;
         SX : constant Real  := -2.0 * BX / 3.0;
         SY : constant Real  := -2.0 * BY / 3.0;
         SA : constant Angle := -2.0 * BA / 3.0;
      begin
         DX := BX;
         while DX <= abs (BX) loop
            DY := BY;
            while DY <= abs (BY) loop
               DA := BA;
               while DA <= abs (BA) loop
                  --  Try_Match (Delta_Pose + Types.Pose'(DX, DY, DA));
                  DA := DA + SA;
               end loop;
               DY := DY + SY;
            end loop;
            DX := DX + SX;
         end loop;
      end;

      --  Finally try suggested:
      Try_Match (Delta_Pose);

      --  Keep the one with min error having > 20% of max_assocs
      declare
         Best_Val   : Float    := 0.0;
         Best_Index : Positive := Matches.Last_Index + 1;
         Required   : constant Natural := 20 * Max_Assoc / 100;
      begin
         for I in Matches.First_Index .. Matches.Last_Index loop
            declare
               New_Val : constant Float := Match_Value (Matches.Element (I));
            begin
               if Matches.Element (I).Assocs >= Required and then
                 Matches.Element (I).Error > 0.000000001 and then -- Needed in simulation
                 New_Val > Best_Val
               then
                  Best_Index := I;
                  Best_Val   := New_Val;
               end if;
            end;
         end loop;

         if Best_Index <= Matches.Last_Index then
            declare
               New_Pose  : constant Types.Pose :=
                             +Compose (+Bot1.Agent.Get_Pose,
                                       +Matches.Element (Best_Index).Result);
            begin
               Log ("Delta from MbICP is " &
                    To_String (Matches.Element (Best_Index).Result),
                    Trace.Debug);
               Log ("Matched with value " & To_String (Best_Val, 7),
                    Trace.Debug);
               Log ("Matched with assocs " &
                    To_String (Matches.Element (Best_Index).Assocs),
                    Trace.Debug);
               Visor_Data.Get_Link (This.Data).Send
                 (Network.New_Address (Bot2.Agent.Get_Id, Gui_Channel),
                  Network.Messages.Set_Pose (New_Pose));
            end;
         else
            Log ("No satisfactory match found!", Warning);
         end if;
      end;
   end Match_Two_Bots;

   ---------------------
   -- Mission_Acquire --
   ---------------------

   procedure Mission_Acquire (This : in out Object) is
      Robots : constant Visor_Data.Robot_Array :=
                 This.Data.Get_Robots;
   begin
      This.Data.Get_Mission.all.Clear;

      for I in Robots'Range loop
         This.Data.Get_Mission.all.Add_Tasks (Robots (I).Agent.Get_Tasks);
      end loop;

      This.Data.Get_Mission.Print_Summary;
   end Mission_Acquire;

   -------------------
   -- Mission_Start --
   -------------------

   procedure Mission_Start (This : in out Object) is
   begin
      Mission_Start (This, This.Data.Get_Mission.Get_Plan);
   end Mission_Start;

   -------------------
   -- Mission_Start --
   -------------------
   --  If there's an annealer plugin, the mission is passed to it.
   --  Tasks are auctioned in any case.
   procedure Mission_Start (This : in out Object;
                            Plan : in     Sancta.Plan.Object)
   is
      T : constant Sancta.Tasks.Containers.Vectors.Vector :=
            Sancta.Tasks.Utils.To_Vector
              (Plan.Enumerate_Tasks
                 (Pending   => True,
                  Primitive => True));
      use Sancta.Plan_Node;

      --  Database : constant Distributed.Datastore.Object_Access := null;
--                   Sancta.Plugin.Shared_Database.Get;
      use type Distributed.Datastore.Object_Access;
   begin
--        if Database /= null then
--           declare
--              Shared_Context_Key : Distributed.Object_Key renames
--                Plugin.Annealer.Shared_Context_Key;
--              Danneal  : Distributed.Types.Danneal;
--              New_Plan : Sancta.Plan.Object := Plan;
--              Success  : Boolean;
--
--              procedure Do_It (Key   : in     Distributed.Object_Key;
--                               Value : in out Distributed.Object_Data'Class;
--                               Meta  : in     Distributed.Object_Metadata)
--              is
--                 pragma Unreferenced (Key, Meta);
--                 Danneal  : Distributed.Types.Danneal renames
--                   Distributed.Types.Danneal (Value);
--              begin
--                 Danneal.Plan       := New_Plan;
--                 Cost_Proxy.Object (Danneal.Agent_Costs.Ref.all).Add_Tasks
--                   (New_Plan.Enumerate_Tasks (Primitive => True,
--                                              Finished  => True,
--                                              Pending   => True));
--                 Cost_Proxy.Object
--                   (Danneal.Agent_Costs.Ref.all).Add_Tasks
--                   (New_Plan.Enumerate_Tasks (Primitive => True,
--                                              Pending   => True,
--                                              Finished  => True));
--                 Danneal.Ass_Cost   := Sancta.Infinite;
--                 Danneal.Assignment := Sancta.Assignment.Invalid_Assignment;
--              end Do_It;
--           begin
--              Database.Create (Shared_Context_Key, Danneal, Success);
--              if Success then
--                 Log ("Shared context created", Informative, Log_Section);
--              end if;
--
--              Log ("Passing mission to annealers", Informative, Log_Section);
--
--              --  Config plan
--              New_Plan.Add_Method (Methods.Explore_Segment_Expansion.Instance);
--              New_Plan.Add_Method (Methods.Choose_Entry_Point.Instance);
--
--              New_Plan := New_Plan.Inflate;
--              New_Plan.Print_Tree_Summary;
--
--              Database.Update (Shared_Context_Key,
--                               Do_It'Access,
--                               Success,
--                               Tout => 2.0);
--              if Success then
--                 Log ("Shared new mission", Trace.Debug, Log_Section);
--              else
--                 Log ("Failed to share new mission", Warning, Log_Section);
--              end if;
--           end;
--        end if;

      Log ("Launching mission tasks!", Informative, Log_Section);
      --  First, assigned tasks:
      for I in T.First_Index .. T.Last_Index loop
         declare
            Subplan : constant Sancta.Plan.Subplan :=
                        Plan.Get_Node (T.Element (I).Get_Id);
         begin
            if Get_Owner (Subplan) /= "" then
               declare
                  Bot : constant Visor_Data.Robot_Access :=
                          This.Data.Get_Robot (Get_Owner (Subplan));
               begin
                  if Bot /= null then
                     Log ("Sending task to " & Get_Owner (Subplan) &
                          ": " & T.Element (I).To_String,
                          Informative);
                     Bot.Agent.Add_Task (T.Element (I));
                  else
                     Log ("Skipping task for unknown bot " &
                          Get_Owner (Subplan),
                          Informative);
                  end if;
               end;
            end if;
         end;
      end loop;
      --  Actually send:
      declare
         Bots : constant Visor_Data.Robot_Array := This.Data.Get_Robots;
      begin
         for I in Bots'Range loop
            --  Directly assign.
            This.Send_Message
              (Bots (I),
               Network.Messages.Set_Tasks (Bots (I).Agent.Get_Tasks));
         end loop;
      end;
      --  Now, tasks to be auctioned:
      for I in T.First_Index .. T.Last_Index loop
         declare
            Subplan : constant Sancta.Plan.Subplan :=
                        Plan.Get_Node (T.Element (I).Get_Id);
         begin
            if Get_Owner (Subplan) = "" then
               Put_Line ("Auctioning task: " & T.Element (I).To_String);
               This.Data.Add_Task_For_Auction (T.Element (I));
            else
               Put_Line ("Skipping auction task: " & T.Element (I).To_String);
            end if;
         end;
      end loop;
   end Mission_Start;

   ------------------
   -- Prepend_Task --
   ------------------

   procedure Prepend_Task (This : in out Object;
                           Bot  : in     Visor_Data.Robot_Access;
                           Job  : in     Sancta.Tasks.Object'Class)
   is
   begin
      if Bot /= null then
         declare
            New_Tasks : Sancta.Tasks.Containers.Lists.List :=
                          Bot.Agent.Get_Tasks;
         begin
            New_Tasks.Prepend (Job);
            Visor_Data.Get_Link (This.Data).Send
              (Network.New_Address (Bot.Agent.Get_Id, Gui_Channel),
               Network.Messages.Set_Tasks (New_Tasks));
         end;
      end if;
   end Prepend_Task;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Log ("0: Deselect robots", Always);
      Log ("1 - 4: Select robot", Always);
      Log ("", Always);
      Log ("F5 - F8: View modes", Always);
      Log ("F9 - F12: Start mission 1 - mission 4", Always);
      Log ("", Always);
      Log ("Q: Drag robot", Always);
      Log ("W: Goto pose", Always);
      Log ("E: Guide robot", Always);
      Log ("R: Auction task", Always);
      Log ("", Always);
      Log ("T: Toggle anneal executor", Always);
      Log ("Y: Request assignment criterion", Always);
      Log ("A: Halt robot", Always);
      Log ("S: Remove first robot task", Always);
      Log ("<space>: Clear all robot tasks", Always);
      Log ("", Always);
      Log ("H: Halt all robots", Always);
      Log ("J: Remove first task from all robots", Always);
      Log ("L: Relative location match", Always);
      Log ("", Always);
      Log ("Z: Decrease angle", Always);
      Log ("X: Increase angle", Always);
      Log ("C: Auto-align", Always);
      Log ("", Always);
      Log ("V: Prompt for pose", Always);
      Log ("B: Sight toggling", Always);
      Log ("", Always);
      Log ("N: Acquire mission from robots", Always);
      Log ("M: Send mission to robots", Always);
   end Print_Help;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (This : in out Object) is
   begin
      if This.Redraw_Timer.Elapsed >= Minimum_Redraw_Wait then
         Queue_Draw (This.Area);
         This.Redraw_Timer.Reset;
      end if;
   end Redraw;

   -----------------------
   -- Remove_First_Task --
   -----------------------

   procedure Remove_First_Task (This : in out Object;
                                Bot  : in     Visor_Data.Robot_Access) is
   begin
      if Bot /= null then
         declare
            Tasks : Sancta.Tasks.Containers.Lists.List := Bot.Agent.Get_Tasks;
         begin
            Tasks.Delete_First;
            Visor_Data.Get_Link (This.Data).Send
              (Network.New_Address (Bot.Agent.Get_Id, Gui_Channel),
               Network.Messages.Set_Tasks (Tasks));
         end;
      end if;
   end Remove_First_Task;

   --------------------
   -- Robot_Drag_End --
   --------------------

   procedure Robot_Drag_End
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access)
   is
      pragma Unreferenced (Widget, Args);
      Thix   : constant Visor_General_View.Object_Access :=
                 Visor_General_View.Object_Access (This);
   begin
      case Thix.Interaction is
         when Drag_Robots =>
            if Thix.Dragging then
               --  Send updated pose to robot:
               --  Put_Line ("Sending update");
               Log ("Drag_End: Sending updated pose to " &
                    Network.Image (Network.New_Address
                    (Thix.Selected_Bot.Agent.Get_Id, Gui_Channel)),
                    Debug, Log_Section);
               Visor_Data.Get_Link (Thix.Data).Send
                 (Network.New_Address
                    (Thix.Selected_Bot.Agent.Get_Id, Gui_Channel),
                  Network.Messages.Set_Pose (Thix.Drag_Pose));
               Thix.Dragging := False;
            end if;

         when Guide_Robot =>
            Thix.Dragging := False;

         when Go_To_Place | Auction_Goal => null;
      end case;

      Thix.Dragging := False;
   end Robot_Drag_End;


   --------------------------
   -- Robot_Drag_Happening --
   --------------------------

   function Robot_Drag_Happening
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access) return Boolean
   is
      pragma Unreferenced (Widget);
      Thix    : constant Visor_General_View.Object_Access :=
            Visor_General_View.Object_Access (This);
--      Context : constant Drag_Context := Drag_Context (To_C_Proxy (Args, 1));
      X       : constant Gint := To_Gint (Args, 2);
      Y       : constant Gint := To_Gint (Args, 3);
      P       : constant Agpl.Gdk.Float_Vector :=
                  Thix.Drawer.Transform_Back ((Float (X), Float (Y), 1.0));
      Pose    : constant Types.Pose := (+P (1), +P (2), 0.0);

      No_More_Drag_Events   : constant Boolean := True;
      Want_More_Drag_Events : constant Boolean := False;
   begin
      case Thix.Interaction is
         when Drag_Robots =>
            if not Thix.Dragging then
               if Thix.Selected_Bot /= null and then Thix.Drag_Kind /= Invalid then
                  Thix.Dragging  := True;
               else
                  return No_More_Drag_Events;
               end if;
            end if;

            if Thix.Dragging then
               case Thix.Drag_Kind is
               when Xy =>
                  Thix.Drag_Pose.X := + P (1);
                  Thix.Drag_Pose.Y := + P (2);
               when Ang =>
                  Thix.Drag_Pose.A := Angle (Float (Thix.Drag_A) +
                                               Float (X - Thix.Drag_X) / 20.0);
               when others => null;
               end case;
               Redraw (Thix.all);
            end if;

         when Guide_Robot =>
            if Thix.Selected_Bot = null then
               return No_More_Drag_Events;
            end if;

            Visor_Data.Get_Link (Thix.Data).Send
              (Network.New_Address
                 (Thix.Selected_Bot.Agent.Get_Id,
                  Gui_Channel),
               Network.Messages.Set_Task
                 (Tasks.Goto_Pose.Create (Pose      => Pose,
                                          Use_Angle => False)));
            delay 0.1; -- Do not saturate link!

         when Go_To_Place | Auction_Goal => null;
      end case;

      return Want_More_Drag_Events;
   exception
      when E : others =>
         Log ("Drag_Happening: " & Report (E), Warning);
         return False;
   end Robot_Drag_Happening;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message (This : in out Object;
                           Bot  : in     Visor_Data.Robot_Access;
                           Msg  : in     Network.Message'Class)
   is
   begin
      if Bot /= null then
         Visor_Data.Get_Link (This.Data).Send
           (Network.New_Address (Bot.Agent.Get_Id, Gui_Channel), Msg);
      else
         Log ("General_View: Discarding message for unknown bot", Warning);
      end if;
   end Send_Message;

   ----------------------
   -- Set_Criterion_Ok --
   ----------------------

   procedure Set_Criterion_Ok
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access)
   is
      use Dialog_Set_Criterion_Pkg;
      type Dialog_Access is access all Dialog_Set_Criterion_Record;
      Dialog : constant Dialog_Access := Dialog_Access (Get_Toplevel (Widget));

      pragma Unreferenced (Widget, Args, This);
   begin
--        Plugin.Annealer.Set_Criterion
--          ((Float'Value (Get_Text (Dialog.Minmax)),
--            Float'Value (Get_Text (Dialog.Minsum))));

      Destroy (Dialog);
   end Set_Criterion_Ok;

   -----------------
   -- Set_Pose_Ok --
   -----------------

   procedure Set_Pose_Ok
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args;
      This   : Visor_Widget.Object_Access)
   is
      use Dialog_Set_Pose_Pkg;
      type Dialog_Access is access all Dialog_Set_Pose_Record;
      Dialog : constant Dialog_Access := Dialog_Access (Get_Toplevel (Widget));

      Thix : constant Object_Access := Object_Access (This);
      pragma Unreferenced (Args);
      Pose   : Types.Pose;
   begin
      Pose.X := Types.Real'Value (Get_Text (Dialog.Entry_X));
      Pose.Y := Types.Real'Value (Get_Text (Dialog.Entry_Y));
      Pose.A := Types.Angle'Value (Get_Text (Dialog.Entry_A));

      Send_Message (Thix.all, Thix.Selected_Bot,
                    Network.Messages.Set_Pose (Pose));

      Destroy (Dialog);
   exception
      when others =>
         Log ("Error parsing new pose", Warning);
   end Set_Pose_Ok;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Visor_Factory.Register (Create'Access, View_name);
   end Register;

end Sancta.Gui.Visor_General_View;
