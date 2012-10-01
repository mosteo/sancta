with Sancta.Tasks.Positioned;

with Sancta.Containers; use Sancta.Containers;
with Agpl.Gdk.Constants;
with Agpl.Gdk.Drawer;
--  with Agpl.Gdk.Drawer_Arc;
with Agpl.Gdk.Drawer_Figures;
with Agpl.Gdk.Drawer_Point;
with Agpl.Gdk.Drawer_Rectangle;
with Agpl.Gdk.Managed.Drawing_Area;
--  with Agpl.Gdk.Modal_Drawing_Area;
with Agpl.Gdk.Palette;

with Sancta.Tasks;
with Sancta.Tasks.Containers;

with Gdk.Drawable; use Gdk.Drawable;

with Ada.Containers.Ordered_Maps;

package body Sancta.Draw_Mtsp is

--   type String_Access is access all String;

   Color_List : constant array (1 .. 100) of access String :=
                  (1      => new String'("red"),
                   2      => new String'("#00aa00"),
                   3      => new String'("#0000ff"),
                   4      => new String'("magenta"),
                   others => new String'("black"));

   procedure Do_Draw (Drawable : in Gdk_Drawable;
                      Poses    : in Pose_Array;
                      Tour     : in Normal_Tour);

--     type Draw_Solution (Last : Natural) is
--       new Agpl.Gdk.Managed.Drawing_Area.Draw_Code with
--        record
--           Poses : Pose_Array (1 .. Last);
--           Tour  : Normal_Tour; -- The solution to draw.
--        end record;
--
--     -------------
--     -- Execute --
--     -------------
--
--     procedure Execute (This : in out Draw_Solution) is
--     begin
--        Do_Draw (This.Drawable,
--                 This.Poses,
--                 This.Tour);
--     end Execute;

   -------------
   -- Do_Draw --
   -------------

   procedure Do_Draw (Drawable : in Gdk_Drawable;
                      Poses    : in Pose_Array;
                      Tour     : in Normal_Tour)
   is
      package Colors renames Agpl.Gdk.Constants.Colors;

      MinX, MinY : Float := Float'Last;
      MaxX, MaxY : Float := Float'First;

      use Agpl.Gdk;
      use Agpl.Gdk.Palette;
      Pal        : aliased Palette.Object;
      Draw       : Drawer.Object;
   begin
      --  Get limits
      for I in Poses'Range loop
         MaxX := Float'Max (MaxX, Float (Poses (I).X));
         MaxY := Float'Max (MaxY, Float (Poses (I).Y));

         MinX := Float'Min (MinX, Float (Poses (I).X));
         MinY := Float'Min (MinY, Float (Poses (I).Y));
      end loop;

      --  Add 1% margin:
      declare
         Factor : constant Float := Float'Max (MaxX - MinX, MaxY - MinY) * 0.01;
      begin
         MaxX := MaxX + Factor;
         MaxY := MaxY + Factor;
         MinX := MinX - Factor;
         MinY := MinY - Factor;
      end;

      Pal.Set_Drawable (Drawable);
      Draw.Set_Drawable (Drawable);

      Drawer.Draw_Begin (Draw);

      --  Draw border
      Drawer.Draw (Draw, Drawer_Rectangle.Create
                     (Get_Color (Pal, Colors.White),
                      MinX, MinY, MaxX, MaxY, Fill => True));
      Drawer_Figures.Draw_Segment (MinX, MinY, MinX, MaxY,
                                   Get_Color (Pal, Colors.Black),
                                   Draw);
      Drawer_Figures.Draw_Segment (MinX, MaxY, MaxX, MaxY,
                                   Get_Color (Pal, Colors.Black),
                                   Draw);
      Drawer_Figures.Draw_Segment (MaxX, MaxY, MaxX, MinY,
                                   Get_Color (Pal, Colors.Black),
                                   Draw);
      Drawer_Figures.Draw_Segment (MaxX, MinY, MinX, MinY,
                                   Get_Color (Pal, Colors.Black),
                                   Draw);

      --  Draw cities
      for I in Poses'Range loop
         Drawer_Figures.Draw_Robot (+Poses (I).X,
                                    +Poses (I).Y,
                                    Float (Poses (I).A),
                                    Get_Color (Pal, "#aaaaaa"),
                                    Draw,
                                    1.5);
--           Drawer.Draw (Draw, Drawer_Arc.Create_Circle
--                          (Get_Color (Pal, "#aaaaaa"),
--                           +Poses (I).X, +Poses (I).Y, 2.0,
--                           Fill => True));
      end loop;

      --  Draw homes
      for I in 1 .. Tour.Last loop
         declare
            Home : constant Integer := Integer (Tour.City (I, 1));
         begin
            Drawer_Figures.Draw_Robot (+Poses (Home).X,
                                       +Poses (Home).Y,
                                       Float (Poses (Home).A),
                                       Get_Color (Pal, Color_List (Integer (I)).all),
                                       Draw,
                                       1.5);
--              Drawer.Draw (Draw, Drawer_Arc.Create_Circle
--                             (Get_Color (Pal, Color_List (Integer (I)).all),
--                              +Poses (Home).X,
--                              +Poses (Home).Y,
--                              2.0,
--                              Fill => True));
         end;
      end loop;

      --  Draw tours
      for Salesman in 1 .. Tour.Last loop
         for Stage in 1 .. Tour.Last (Salesman) - 1 loop
            Drawer_Figures.Draw_Segment
              (+Poses (Integer (Tour.City (Salesman, Stage))).X,
               +Poses (Integer (Tour.City (Salesman, Stage))).Y,
               +Poses (Integer (Tour.City (Salesman, Stage + 1))).X,
               +Poses (Integer (Tour.City (Salesman, Stage + 1))).Y,
               Get_Color (Pal, Color_List (Integer (Salesman)).all),
               Draw);
         end loop;
         if False then -- Return to home, not drawn.
            Drawer_Figures.Draw_Segment
              (+Poses (Integer (Tour.City (Salesman, Tour.Last (Salesman)))).X,
               +Poses (Integer (Tour.City (Salesman, Tour.Last (Salesman)))).Y,
               +Poses (Integer (Tour.City (Salesman, 1))).X,
               +Poses (Integer (Tour.City (Salesman, 1))).Y,
               Get_Color (Pal, Colors.Black), -- Draw in black the returning line
               Draw);
         end if;
      end loop;

      --  Draw city center points
      for I in Poses'Range loop
         Drawer.Draw (Draw, Drawer_Point.Create (Get_Color (Pal, "Black"),
                                                 +Poses (I).X,
                                                 +Poses (I).Y));
      end loop;

      --  Show
      Drawer.Draw_End (Draw);
   end Do_Draw;

   procedure From_Agent (This : in Agent_Proxy.Object) is
      use Sancta.Agent.Containers.Lists;
      L : List;
   begin
      L.Append (This);
      From_Tasks (L);
   end From_Agent;

   ---------------------
   -- From_Assignment --
   ---------------------

   procedure From_Assignment (This : in Sancta.Assignment.Object;
                              Title : String := "")
   is
   begin
      From_Tasks (Sancta.Assignment.Get_Agents (This), Title);
   end From_Assignment;

   ----------------
   -- From_Poses --
   ----------------

--     procedure From_Poses
--       (Poses : in Pose_Array;
--        Tour  : in Normal_Tour;
--        Title : in String := "")
--     is
--        --  package MDA renames Agpl.Gdk.Modal_Drawing_Area;
--
--        use type Types.Real;
--
--        --  Area : MDA.Object;
--
--        -------------------
--        -- Draw_Solution --
--        -------------------
--
--  --        procedure Draw_Solution (Drawable : in Gdk_Drawable)
--  --        is
--  --        begin
--  --           Do_Draw (Drawable, Poses, Tour);
--  --        end Draw_Solution;
--
--     begin
--        --  Use this for modal drawing
--        --  Area.Show (Draw_Solution'Access);
--
--        --  Use this for non-modal drawing:
--        declare
--           Dr : Sancta.Draw_Mtsp.Draw_Solution (Last => Poses'Length);
--        begin
--           Dr.Poses := Poses;
--           Dr.Tour  := Tour;
--
--           Agpl.Gdk.Managed.Drawing_Area.Show (Dr, "Solution " & Title);
--        end;
--     end From_Poses;

   ----------------
   -- From_Tasks --
   ----------------

   --  We'll transform into a From_Poses apt form.
   procedure From_Tasks (Agents : in Sancta.Agent.Containers.Lists.List;
                         Title  : in String := "") is

      package Id_To_Idx is new Ada.Containers.Ordered_Maps (Sancta.Tasks.Task_Id,
                                                            Natural,
                                                            Sancta.Tasks."<");

      Index : Id_To_Idx.Map;

      Num_Tasks  : Natural          := 0;
      Num_Agents : constant Natural := Natural (Agents.Length);

      use Sancta.Agent.Containers.Lists;
      use Sancta.Tasks.Containers;

      Counter : Positive := 1;

      ------------------
      -- Add_To_Index --
      ------------------

      --  Here we count how many tasks there are,
      --  and create an index from Task_Id --> Positive series
      procedure Add_To_Index (X : in Sancta.Agent.Containers.Lists.Cursor) is
         Tasks : constant Task_Lists.List   := Element (X).Get_Tasks;
         I     :          Task_Lists.Cursor := Tasks.First;
         use Task_Lists;
      begin
         Num_Tasks := Num_Tasks + Natural (Tasks.Length);

         while Has_Element (I) loop
            Index.Include (Element (I).Get_Id, Counter);
            Counter := Counter + 1;
            Next (I);
         end loop;
      end Add_To_Index;

   begin
      Agents.Iterate (Add_To_Index'Access);

      --  At this point, Num_Tasks contains the proper number, and we have the
      --  index. Note that there are additional poses: the starting ones.
      --  We'll place the tour starting cities at the end.

      declare
         Poses : Types.Pose_Array (1 .. Num_Tasks + Num_Agents);
         Tour  : Normal_Tour := Create (Salesmen (Num_Agents));
         I     : Sancta.Agent.Containers.Lists.Cursor := Agents.First;
         Idx   : Natural := Poses'Last - Num_Agents + 1; -- Index to the starting city
                                                         --  of the salesman.
         Man   : Natural := 1;                           -- Index of the current salesman.
      begin
         --  Append the initial cities and poses:
         while Has_Element (I) loop
            Poses (Idx) := Agent_Proxy.Object (Element (I)).Get_Pose;
            Append_City (Tour, Salesmen (Man), Cities (Idx));

            --  Append each task
            declare
               Tasks : constant Task_Lists.List   := Element (I).Get_Tasks;
               J     :          Task_Lists.Cursor := Tasks.First;
               use Task_Lists;
            begin
               while Has_Element (J) loop
                  declare
                     Job : Sancta.Tasks.Positioned.Object renames
                       Sancta.Tasks.Positioned.Object (Element (J));
                     Pos : constant Positive :=
                             Id_To_Idx.Element
                               (Id_To_Idx.Find
                                  (Index,
                                   Sancta.Tasks.Positioned.Get_Id (Job)));
                  begin
                     Poses (Pos) := Job.Pose;
                     Append_City (Tour, Salesmen (Man), Cities (Pos));
                  end;
                  Next (J);
               end loop;
            end;
            Idx := Idx + 1;
            Man := Man + 1;
            Next (I);
         end loop;
         From_Poses (Poses, Tour, Title);
      end;
   end From_Tasks;

end Sancta.Draw_Mtsp;
