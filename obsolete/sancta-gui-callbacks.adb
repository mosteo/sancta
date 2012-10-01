with Sancta.Gui.Main;
--  with Sancta.Tasks.Types;
with Sancta.Types;
with Sancta.Types.String_Pose_Maps;

with Sancta.Gap.Safe_Array;
with Sancta.Gap.Vector;
with Sancta.Gap.Vector.Map;
with Sancta.Map;
--  with Sancta.Map.Grid.Draw;
with Agpl.Gdk;
with Agpl.Gdk.Constants;
with Agpl.Gdk.Controlled_Gc;
with Agpl.Gdk.Drawer;
with Agpl.Gdk.Drawer_Arc;
with Agpl.Gdk.Drawer_Figures;
with Agpl.Gdk.Drawer_Line;
with Agpl.Gdk.Drawer_Point;
with Agpl.Gdk.Drawer_Rectangle;
with Agpl.Gdk.Drawer_Segment;
with Agpl.Gdk.Utils; use Agpl.Gdk.Utils;
use  Agpl.Gdk;

--  with Agpl.Strings; use Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

with Gdk.Color;          use Gdk.Color;
with Gdk.Drawable;       use Gdk.Drawable;
with Gdk.Gc;             use Gdk.Gc;
with Gdk.Window;         use Gdk.Window;
with Gtk.Tree_Model;     use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Pango.Layout;       use Pango.Layout;

package body Sancta.Gui.Callbacks is

   use type Sancta.Types.Real;
   use type Controlled_Gc.Object;

   --  Forward declarations

   ---------------------
   -- Console_Changed --
   ---------------------

   procedure Console_Changed
     (Widget : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class)
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
   begin
      Get_Selected (Widget, Model, Iter);
      Path := Get_Path (Model, Iter);
      Gui.Main.Display_Canvas (Natural'Value (To_String (Path)));
      Path_Free (Path);
   end Console_Changed;

   -----------------
   -- Expose_Gaps --
   -----------------

--     function Expose_Gaps (Widget : access Gtk_Widget_Record'Class;
--                           Event  :        Gdk.Event.Gdk_Event_Expose;
--                           Data   :        Gui.User_Data_Handle.Object)
--                           return          Boolean
--     is
--        pragma Unreferenced (Event);
--        use Gui.User_Data_Handle;
--        use Sancta.Gap.Safe_Array;
--        G  : Sancta.Gap.Object_Array renames
--          Get (Tasks.Types.Wander_For_Gaps_Info (Val (Data)).Gaps);
--        Ok : Boolean;
--     begin
--        --  Put_Line ("Found" & Natural'Image (G'Length) & " gaps");
--        for I in G'Range loop
--           Sancta.Gap.Dump (G (I));
--        end loop;
--
--        declare
--           Draw  : Agpl.Gdk.Drawer.Object; use Agpl.Gdk;
--           W     : constant Gdk_Window := Get_Window (Widget);
--           Red   : Gdk_Color           := Agpl.Gdk.Constants.Red;
--           White : Gdk_Color           := Agpl.Gdk.Constants.White;
--           Black : Gdk_Color           := Agpl.Gdk.Constants.Black;
--           Blue  : Gdk_Color           := Agpl.Gdk.Constants.Blue;
--           Silver: Gdk_Color           := Agpl.Gdk.Constants.Silver;
--           Gc    : Gdk_Gc;
--           Gc2   : Gdk_Gc;
--           Gc_Silver : Gdk_Gc;
--           Gc_Blue   : Gdk_Gc;
--           Gc_Current: Gdk_Gc;
--        begin
--           Alloc_Color (Get_Default_Colormap, White, Success => Ok);
--           Alloc_Color (Get_Default_Colormap, Red,   Success => Ok);
--           Alloc_Color (Get_Default_Colormap, Black, Success => Ok);
--           Alloc_Color (Get_Default_Colormap, Blue,  Success => Ok);
--           Alloc_Color (Get_Default_Colormap, Silver, Success => Ok);
--
--           if Ok then
--              Set_Background (W, White);
--           end if;
--
--           Gdk_new (Gc, W);
--           Gdk_new (Gc2, W);
--           Gdk_new (Gc_Silver, W);
--           Gdk_new (Gc_Blue, W);
--
--           Set_Foreground      (Gc, Red);
--           Set_Line_Attributes (Gc,
--                                3,
--                                Line_Solid,
--                                Cap_Round,
--                                Join_Round);
--
--           Set_Foreground      (Gc_Blue, Blue);
--           Set_Line_Attributes (Gc_Blue,
--                                3,
--                                Line_Solid,
--                                Cap_Round,
--                                Join_Round);
--
--           Set_Foreground      (Gc2, Black);
--           Set_Line_Attributes (Gc2,
--                                0,
--                                Line_Solid,
--                                Cap_Round,
--                                Join_Round);
--
--           Set_Foreground      (Gc_Silver, Silver);
--           Set_Line_Attributes (Gc_Silver,
--                                0,
--                                Line_Solid,
--                                Cap_Round,
--                                Join_Round);
--
--           Drawer.Set_Transformation (Draw, Agpl.Gdk.Look_Up);
--           Drawer.Draw_Begin (Draw);
--           Drawer.Set_Drawable (Draw, W);
--
--           declare
--              use Sancta.Gap;
--           begin
--              for I in G'Range loop
--                 Drawer.Draw (Draw,
--                              Drawer_Line.Create
--                                (Gc_Silver,
--                                 Get_Start (G (I)) (1),
--                                 Get_Start (G (I)) (2),
--                                 Get_End   (G (I)) (1),
--                                 Get_End   (G (I)) (2)));
--                 if Get_Kind (G (I)) = Occlusive then
--                    Gc_Current := Gc;
--                 else
--                    Gc_Current := Gc_Blue;
--                 end if;
--                 Drawer.Draw (Draw,
--                              Drawer_Segment.Create
--                                (Gc_Current,
--                                 Get_Start (G (I)) (1),
--                                 Get_Start (G (I)) (2),
--                                 Get_End   (G (I)) (1),
--                                 Get_End   (G (I)) (2)));
--              end loop;
--              Drawer.Draw (Draw, Drawer_Arc.Create_Circle (Gc_Silver, 0.0, 0.0, 0.35));
--              Drawer.Draw (Draw, Drawer_Segment.Create (Gc2, 0.0, 0.0, 0.5, 0.0));
--              Drawer.Draw (Draw, Drawer_Segment.Create (Gc2, 0.0, -0.3, 0.0, 0.3));
--              Drawer.Draw (Draw, Drawer_Segment.Create (Gc2, 0.0, -0.3, 0.5, 0.0));
--              Drawer.Draw (Draw, Drawer_Segment.Create (Gc2, 0.0, 0.3, 0.5, 0.0));
--              Drawer.Draw (Draw, Drawer_Point.Create (Gc2, 8.0, -8.0));
--              Drawer.Draw (Draw, Drawer_Point.Create (Gc2, 8.0, 8.0));
--           end;
--           Drawer.Draw_End (Draw);
--
--           Unref (Gc);        -- Destroy once used
--           Unref (Gc2);
--           Unref (Gc_Silver);
--           Unref (Gc_Blue);
--        end;
--
--        return True;
--     end Expose_Gaps;

   -------------------
   -- Pursuit_Click --
   -------------------

--     function Pursuit_Click (Widget : access Gtk_Widget_Record'Class;
--                             Event  :        Gdk.Event.Gdk_Event_Expose;
--                             Data   :        Tasks.Types.Smart_Pursuit_Info.Object)
--                             return          Boolean
--     is
--     begin
--        return True;
--     end Pursuit_Click;

   --------------------
   -- Pursuit_Expose --
   --------------------

--     function Pursuit_Expose (Widget : access Gtk_Widget_Record'Class;
--                              Event  :        Gdk.Event.Gdk_Event_Expose;
--                              Data   :        Tasks.Types.Smart_Pursuit_Info.Object)
--                              return          Boolean
--     is
--        pragma Unreferenced (Event);
--        use Tasks.Types.Smart_Pursuit_Info;
--
--        W    : constant Gdk_Window := Get_Window (Widget);
--        Info : Tasks.Types.Pursuit_Info renames Val (Data);
--
--        Gc_Black : Controlled_Gc.Object (W);
--        Gc_Blue  : Controlled_Gc.Object (W);
--        Gc_Red   : Controlled_Gc.Object (W);
--        Gc_Gray  : Controlled_Gc.Object (W);
--        Gc_Green : Controlled_Gc.Object (W);
--        Gc_White : Controlled_Gc.Object (W);
--        Gc_Light_Red   : Controlled_Gc.Object (W);
--        Gc_Light_Green : Controlled_Gc.Object (W);
--
--        Bg_White : Gdk_Color := Agpl.Gdk.Constants.White;
--
--        Ok       : Boolean;
--
--        use Agpl.Gdk.Constants;
--
--        -------------------------
--        -- Pursuit_Map_Palette --
--        -------------------------
--
--        function Pursuit_Map_Palette (This : in Sancta.Map.Observations'Class)
--                                      return Gdk_Gc
--        is
--           use Tasks.Types;
--           Ob : Pursuit_Observations renames Pursuit_Observations (This);
--        begin
--           case Ob.Value is
--              when Clean =>
--                 return +Gc_Light_Green;
--              when Contaminated =>
--                 return +Gc_Light_Red;
--              when Obstacle =>
--                 return +Gc_Black;
--           end case;
--        end Pursuit_Map_Palette;
--
--     begin
--        Set_Color (Gc_Black, Black, W);
--        Set_Color (Gc_Blue,  Blue, W);
--        Set_Color (Gc_Red,   Red, W);
--        Set_Color (Gc_Gray,  Gray, W);
--        Set_Color (Gc_Green, Green, W);
--        Set_Color (Gc_White, White, W);
--
--        Set_Color (Gc_Light_Green, Parse ("#e0ffe0"), W);
--        Set_Color (Gc_Light_Red,   Parse ("#ffe0e0"), W);
--
--        Alloc_Color (Get_Default_Colormap, Bg_White, Success => Ok);
--        pragma Assert (Ok);
--        Set_Background (W, Bg_White);
--        Clear (W);
--
--        declare
--           use Drawer_Figures;
--           Draw : Drawer.Object;
--        begin
--           Draw.Set_Drawable (W);
--           Draw.Draw_Begin;
--
--           --  Draw background pursuit status:
--           Sancta.Map.Grid.Draw (Info.Map, Draw, Pursuit_Map_Palette'Access);
--
--           --  Draw crosses at the corners of the working area:
--           Draw_Plus ( + Info.P1.X, + Info.P1.Y, + Gc_Gray, Draw);
--           Draw_Plus ( + Info.P2.X, + Info.P2.Y, + Gc_Gray, Draw);
--           Draw_Grid_Points ( + Info.P1.X, + Info.P1.Y,  + Info.P2.X, + Info.P2.Y,
--                             + Gc_Gray, Draw);
--
--           --  Draw gaps seen by each robot:
--           declare
--              use Sancta.Gap.Vector.Map;
--              procedure Draw_Gaps (I : in Cursor) is
--                 V : Sancta.Gap.Vector.Object renames Element (I);
--              begin
--                 for J in V.First .. V.Last loop
--                    Draw_Gap (V.Vector (J), + Gc_Red, Draw);
--                 end loop;
--              end Draw_Gaps;
--           begin
--              Info.Gaps.Iterate (Draw_Gaps'Access);
--           end;
--
--           --  Draw robots and goals
--           declare
--              use Types.String_Pose_Maps;
--              procedure Draw_Robots (I : in Cursor) is
--                 P : Sancta.Types.Pose renames Element (I);
--                 Color : Gdk_Gc;
--              begin
--                 if Key (I) = "Ari" then
--                    Color := +Gc_Red;
--                 elsif Key (I) = "Ben" then
--                    Color := +Gc_Green;
--                 elsif Key (I) = "Ced" then
--                    Color := +Gc_Blue;
--                 else
--                    Color := +Gc_Gray;
--                 end if;
--
--                 Draw_Robot ( + P.X, + P.Y, Float (P.A), Color, Draw);
--              end Draw_Robots;
--              procedure Draw_Goals (I : in Cursor) is
--                 P : Sancta.Types.Pose renames Element (I);
--              begin
--                 Draw_Plus ( + P.X, + P.Y, + Gc_Blue, Draw, 0.5);
--              end Draw_Goals;
--           begin
--              Info.Goals.Iterate (Draw_Goals'Access);
--              Info.Poses.Iterate (Draw_Robots'Access);
--           end;
--
--           Draw.Draw_End;
--        end;
--
--        return True;
--     exception
--        when E : others =>
--           Log ("Sancta.Gui.Callbacks.Pursuit_Expose: " & Report (E), Warning);
--           return True;
--     end Pursuit_Expose;

   ----------------
   -- User_Click --
   ----------------

   function User_Click (Widget : access Gtk_Widget_Record'Class;
                        Event  :        Gdk.Event.Gdk_Event_Expose;
                        Data   :        User_Data_Handle.Object)
                        return          Boolean
   is
      pragma Unreferenced (Widget, Event);
      use Gui.User_Data_Handle;
   begin
      return True;
   end User_Click;

   --------------------
   -- User_Data_Draw --
   --------------------

   function User_Data_Draw (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose;
                            Data   :        User_Data_Handle.Object)
                            return          Boolean
   is
      pragma Unreferenced (Event);
      Ok : Boolean;
      W  : constant Gdk_Window := Get_Window (Widget);
      use Agpl.Gdk;
      use Gui.User_Data_Handle;
      use type Controlled_Gc.Object;

      Draw : Drawer.Object;

      --  Colors
      Black : Gdk_Color           := Agpl.Gdk.Constants.Black;
      Blue  : Gdk_Color           := Agpl.Gdk.Constants.Blue;
      Gray  : Gdk_Color           := Agpl.Gdk.Constants.Gray;
      Red   : Gdk_Color           := Agpl.Gdk.Constants.Red;
      White : Gdk_Color           := Agpl.Gdk.Constants.White;

      Gc_Black : Controlled_Gc.Object (W);
      Gc_Blue  : Controlled_Gc.Object (W);
      Gc_Gray  : Controlled_Gc.Object (W);
      Gc_Red   : Controlled_Gc.Object (W);

   begin
      Drawer.Draw_Begin (Draw);
      Drawer.Set_Drawable (Draw, W);

      --  Allocate needed colors:
      Alloc_Color (Get_Default_Colormap, Gray, Success => Ok);
      pragma Assert (Ok);
      Set_Foreground ( + Gc_Gray, Gray);

      Alloc_Color (Get_Default_Colormap, Red, Success => Ok);
      pragma Assert (Ok);
      Set_Foreground ( + Gc_Red, Red);

      Alloc_Color (Get_Default_Colormap, White, Success => Ok);
      pragma Assert (Ok);

      Alloc_Color (Get_Default_Colormap, Black, Success => Ok);
      pragma Assert (Ok);
      Set_Foreground ( + Gc_Black, Black);

      Alloc_Color (Get_Default_Colormap, Blue, Success => Ok);
      pragma Assert (Ok);
      Set_Foreground ( + Gc_Blue, Blue);

      if False then
         null;
      else
         --  No handler for this user data?
         declare
            Text  : constant Pango_Layout := Create_Pango_Layout (Widget,
                                                                "No drawing defined");
            Gc    : Gdk_Gc;
         begin
            Set_Background (W, White);

            Gdk_new (Gc, W);
            Set_Foreground (Gc, Black);

            Draw_Layout (W, Gc, 0, 0, Text);

            Unref (Gc);
         end;
      end if;

      Drawer.Draw_End (Draw);

      return True;
   end User_Data_Draw;

end Sancta.Gui.Callbacks;
