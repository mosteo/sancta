with Sancta.Containers;
with Sancta.Map.Bitmap_Wavefront_4;
with Sancta.Map.Bitmap_Wavefront_8;
with Sancta.Types.Operations;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Tasks.Goto_Pose_Bitmap_Wavefront is

   -------------
   -- To_Grid --
   -------------

   function To_Grid (This : Object;
                     Pose : Types.Pose) return Map.Bitmap.Bit_Location
   is
      use Types;
      Ratio : constant Float := This.Map.Ref.Get_Cell_Size;
   begin
      return (This.Map.Ref,
              X => Abscissa (Real'Floor (Pose.X / Real (Ratio))),
              Y => Ordinate (Real'Floor (Pose.Y / Real (Ratio))));
   end To_Grid;

   -------------
   -- To_Pose --
   -------------

   function To_Pose (This : Object;
                     Loc  : Map.Bitmap.Bit_Location) return Types.Pose
   is
      use Types;
      Ratio : constant Float := This.Map.Ref.Get_Cell_Size;
   begin
      return (X => Real (Loc.X) * Real (Ratio) + Real (Ratio / 2.0),
              Y => Real (Loc.Y) * Real (Ratio) + Real (Ratio / 2.0),
              A => 0.0);
   end To_Pose;

   ------------
   -- Create --
   ------------

   function Create
     (Goal      : Types.Pose;
      Use_Angle : in Boolean     := True;
      Margin_D  : in Types.Real  := 0.5;
      Margin_A  : in Types.Angle := 0.25;
      From      : Types.Pose;
      Map       : Sancta.Map.Bitmap.Smart.Object;
      With_Id   : Tasks.Task_Id := Tasks.No_Task)
      return Object
   is
      use type Tasks.Task_Id;

      This : Object :=
               (Goto_Pose.Create (Goal, Use_Angle, Margin_D, Margin_A) with
                Goal => Goal,
                From => From,
                Map  => Map,
                Route => Sancta.Map.Location_Lists.Empty_List);

      --  Loc : constant Sancta.Map.Bitmap.Bit_Location := This.To_Grid (From);
   begin
      --  Compute route:
      This.Route := Sancta.Map.Bitmap_Wavefront_8
        (This.To_Grid (From),
         This.To_Grid (Goal),
         Map.Ref.all);

      if With_Id /= Tasks.No_Task then
         This.Force_Id (With_Id);
      end if;

      --  Log ("Bot is at " & Loc.X'Img & Loc.Y'Img, Always);

      return This;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Path    : Map.Path;
                    Goal    : Types.Pose;
                    From    : Types.Pose;
                    Map     : Sancta.Map.Bitmap.Smart.Object) return Object
   is

   begin
      return
        (Goto_Pose.Create (Goal) with
         Goal  => Goal,
         From  => From,
         Map   => Map,
         Route => Path);
   end Create;

   ------------------
   -- To_Goto_Pose --
   ------------------

   function To_Goto_Pose (This : Object) return Goto_Pose.Object is
      use Sancta.Map.Bitmap;
      use Sancta.Map.Location_Lists;
   begin
      if Natural (This.Route.Length) <= 2 then
         --  In same or adjacent cell...
         return Goto_Pose.Create (This.Goal,
                                  This.Use_Angle,
                                  This.Margin_Dist,
                                  This.Margin_Angle);
      else
         --  Center of route cell
         return Goto_Pose.Create
           (This.To_Pose
              (Bit_Location
                 (Element (Next (This.Route.First)))),
            This.Use_Angle,
            This.Margin_Dist,
            This.Margin_Angle);
      end if;
   end To_Goto_Pose;

   ------------
   -- Create --
   ------------

   function Create
     (Pose      : in Types.Pose;
      Use_Angle : in Boolean     := True;
      Margin_D  : in Types.Real  := 0.5;
      Margin_A  : in Types.Angle := 0.25)
      return Object
   is
      pragma Warnings (Off);
      This : Object;
      pragma Warnings (On);
      pragma Unreferenced (Pose, Use_Angle, Margin_D, Margin_A);
   begin
      raise Program_Error with "Never should be called";
      return This;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Goal      : Tasks.Goto_Pose.Object'Class;
                    From      : Types.Pose;               -- Starting point
                    Map       : Sancta.Map.Bitmap.Smart.Object; -- Where you are
                    Copy_Id   : Boolean := False)
                    return      Object
   is
      Id : Tasks.Task_Id := Tasks.No_Task;
   begin
      if Copy_Id then
         Id := Goal.Get_Id;
      end if;

      return Create (Goal.Pose,
                     Goal.Use_Angle,
                     Goal.Margin_Dist,
                     Goal.Margin_Angle,
                     From,
                     Map,
                     Id);
   end Create;

   ---------------
   -- Get_Route --
   ---------------

   function Get_Route (This : Object) return Sancta.Map.Location_Lists.List is
   begin
      return This.Route;
   end Get_Route;

   ---------------
   -- Get_Route --
   ---------------

   function Get_Route (This : Object) return Containers.Pose_Vectors.Object is
      V : Containers.Pose_Vectors.Object (First => 1);

      use Sancta.Map.Location_Lists;

      procedure Convert (I : Cursor) is
      begin
         V.Append (This.To_Pose (Map.Bitmap.Bit_Location (Element (I))));
      end Convert;
   begin
      This.Route.Iterate (Convert'Access);
      return V;
   end Get_Route;

   ------------------------
   -- Get_Alternate_Path --
   ------------------------

   function Get_Alternate_Path (This : Map.Bitmap.Object;
                                Ini,
                                Fin  : Types.Pose;
                                Path : Map.Path)
                                return Map.Path
   is
      pragma Ad_Hoc_And_Not_General;
      --  This will only consider paths around the first obstacle found.
      --  A completely general solution I suspect involves some high-level
      --    topological reasoning...

      use Sancta.Map.Bitmap;
      use Sancta.Types;
      use Sancta.Types.Operations;

      Ini_Loc : constant Bit_Location'Class := This.To_Grid (Ini);
      Fin_Loc : constant Bit_Location'class := This.To_Grid (Fin);

      -----------------------
      -- Find_Intersection --
      -----------------------

      function Find_Intersection return Bit_Location is
         Step : constant Real := Real (This.Get_Cell_Size) / 4.0;
         X    : Real := Ini.X;
         Y    : Real := Ini.Y;
         Dx   : constant Real := (Fin.X - Ini.X) / Distance (Ini, Fin) * Step;
         Dy   : constant Real := (Fin.Y - Ini.Y) / Distance (Ini, Fin) * Step;
      begin
         for I in 1 .. Integer (Distance (Ini, Fin) / Step) loop
            X := X + Dx;
            Y := Y + Dy;

            if Bit_Observation
              (This.Get_At (This.To_Grid ((X, Y, 0.0)))).Bit = Obstacle then
               return Bit_Location (This.To_Grid (Pose'(X, Y, 0.0)));
            end if;
         end loop;

         return Bit_Location (Ini_Loc);
      end Find_Intersection;

      ----------------
      -- Mangle_Map --
      ----------------

      function Mangle_Map (Intersect : Bit_Location) return Sancta.Map.Path is
         type Orientations is (Horizontal, Vertical);
         type Sides        is (Left, Right);

         Delta_X  : constant array (Orientations, Left .. Right) of Abscissa :=
                      (Horizontal => (-1, 1), Vertical => (0, 0));
         Delta_Y  : constant array (Orientations, Sides) of Ordinate :=
                      (Horizontal => (0, 0), Vertical => (-1, 1));
         Ori      :          Orientations;

         ---------------
         -- Do_Mangle --
         ---------------

         procedure Do_Mangle (M    : in out Map.Bitmap.Object;
                              Ori  :        Orientations;
                              Side :        Sides)
         is
            X : Abscissa := Intersect.X;
            Y : Ordinate := Intersect.Y;
         begin
            This.Copy_But_Costs (M);
            loop
               X := X + Delta_X (Ori, Side);
               Y := Y + Delta_Y (Ori, Side);
               exit when not M.Within_Bounds (X, Y);
               if This.Loc (X, Y) /= Fin_Loc then
                  M.Set_At (X, Y, Obstacle);
               end if;
            end loop;
            --  M.Print;
         end Do_Mangle;

         ----------------
         -- Intersects --
         ----------------

         function Intersects (Path    : Sancta.Map.Path;
                              Map_Bis : Map.Bitmap.Object) return Boolean
         is
            use Sancta.Map.Location_Lists;
            I : Cursor := Path.First;
         begin
            while Has_Element (I) loop
               if Bit_Observation
                 (Map_Bis.Get_At (Element (I))).Bit = Obstacle then
                  return True;
               end if;
               Next (I);
            end loop;
            return False;
         end Intersects;

      begin
         if Intersect.X = Ini_Loc.X then
            Ori := Horizontal;
         else
            Ori := Vertical;
         end if;

         --  Try both sides, the first one that intersecs will be the good one.
         for Side in Sides loop
            declare
               Map_Bis : Map.Bitmap.Object (This.Vicinity);
            begin
               Do_Mangle (Map_Bis, Ori, Side);
               if Intersects (Path, Map_Bis) then
                  --  The good one: Replan
                  begin
                     return Sancta.Map.Bitmap_Wavefront_8
                       (This.To_Grid (Ini),
                        This.To_Grid (Fin),
                        Map_Bis);
                  exception
                     when Constraint_Error =>
                        --  There was no alternate...
                        Log ("S.M.Bitmap.Mangle: No alternate!",
                             Warning, Log_Section);
                        return Map.Empty_Path;
                  end;
               end if;
            end;
         end loop;
         --  Uh?
         Log ("S.M.Bitmap.Mangle: Uh? No intersections?", Warning, Log_Section);
         return Map.Empty_Path;
      end Mangle_Map;

   begin
      declare
         Intersect : constant Bit_Location'Class := Find_Intersection;
      begin
         --  Log ("Intersect:" & Intersect.X'Img & Intersect.Y'Img, Always);
         if Intersect = Ini_Loc then
            return Map.Empty_Path;
         end if;

         declare
            Alternate : Sancta.Map.Path := Mangle_Map (Bit_Location (Intersect));
         begin
            return Alternate;
         end;
      end;
   end Get_Alternate_Path;

   function Get_Map (This : Object) return Sancta.Map.Bitmap.Smart.Object is
   begin
      return This.Map;
   end Get_Map;

end Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;
