with Agpl.Chronos;
with Agpl.Png,
     Agpl.Search.A_Star,
     Agpl.Ustrings,
     Sancta.Types.Operations;

pragma Elaborate_All (Agpl.Search.A_Star);

package body Sancta.Map is

   --------------
   -- Is_Known --
   --------------

   function Is_Known
     (This : in Object;
      Loc  : in Location'Class)
      return Boolean
   is
   begin
      return This.Loc_Obs.Contains (Loc);
   end Is_Known;

   ------------
   -- Get_At --
   ------------

   function Get_At
     (This : in Object;
      Loc  : in Location'Class)
      return Observation'Class
   is
      use Loc_Obs_Maps;
      I : constant Cursor := This.Loc_Obs.Find (Loc);
   begin
      if Has_Element (I) then
         return Element (I);
      else
         raise No_Data;
      end if;
   end Get_At;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (This : in out Object;
      Loc  : in     Location'Class;
      Obs  : in     Observation'Class)
   is
   begin
      This.Loc_Obs.Include (Loc, Obs);
   end Set_At;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (L, R : Location'Class) return Boolean is
   begin
      return "<" (L, R);
   end Less_Than;

   -----------
   -- Image --
   -----------

   function Image (This : Location) return String is
   begin
      return External_Tag (Location'Class (This)'Tag);
   end Image;

   --------------
   -- From_Png --
   --------------

   procedure From_Png (This : in out Object;
                       File :        String;
                       Load :        Loader)
   is
      use Agpl.Png;
      Png    : Png_File;

      Thix   : constant access Object'Class := Object'Class (This)'Access;
      pragma Unreferenced (This); -- Use Thix always to have dispatching
   begin
      Open (Png, File);

      for Row in Rows'(0) .. Rows (Height (Png) - 1) loop
         for Col in Columns'(0) .. Columns (Width (Png) - 1) loop
            Thix.Set_At
              (Load.Loc (Thix.all, Row, Col),
               Load.Obs ((R =>   Red_Value (Png, +Row, +Col),
                          G => Green_Value (Png, +Row, +Col),
                          B =>  Blue_Value (Png, +Row, +Col))));
         end loop;
      end loop;
   end From_Png;

   ---------
   -- "<" --
   ---------

--     function "<" (L, R : Null_Location_Type) return Boolean is
--        pragma Unreferenced (L, R);
--     begin
--        return False;
--     end "<";

   function To_Vector (This : Location_Lists.List)
                       return Location_Vectors.Vector
   is
      Y : Location_Vectors.Vector;
      procedure Copy (I : Location_Lists.Cursor) is
      begin
         Y.Append (Location_Lists.Element (I));
      end Copy;
   begin
      This.Iterate (Copy'Access);
      return Y;
   end To_Vector;

   -----------------
   -- Remove_Head --
   -----------------

   function Remove_Head (P : Path; Cells : Natural) return Path is
      R     : Path    := P;
   begin
      for I in 1 .. Cells loop
         if not R.Is_Empty then
            R.Delete_First;
         else
            exit;
         end if;
      end loop;
      return R;
   end Remove_Head;

   package Solver is
     new Agpl.Search.A_Star
       (Location_Handle.Object, Costs, 0.0, "<", "+", Costs'Image);

   --------------
   -- Num_Next --
   --------------

   function Num_Next (L : Location_Handle.Object) return Natural is
   begin
      return L.Ref.Num_Neighbors;
   end Num_Next;

   ----------
   -- Next --
   ----------

   function Next (L : Location_Handle.Object;
                  I : Positive)
                  return Location_Handle.Object
   is
      N : constant Location'Class := L.Ref.Neighbor (I);
   begin
      return Location_Handle.Set (N);
   end Next;

   ----------
   -- Cost --
   ----------

   function Cost (Ini, Fin : Location_Handle.Object) return Costs is
   begin
      return Ini.Ref.Real_Cost (Fin.Ref.all);
   end Cost;

   --------------
   -- Estimate --
   --------------

   function Estimate (Ini, Fin : Location_Handle.Object) return Costs is
   begin
      return Ini.Ref.Estimate_Cost (Fin.Ref.all);
   end Estimate;

   -----------
   -- Image --
   -----------

   function Image (L : Location_Handle.Object) return String is
   begin
      return L.Ref.Image;
   end Image;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (P : in out Map.Path; L : Location_Handle.Object) is
   begin
      P.Prepend (L.Ref.all);
   end Prepend;

   procedure Best is new Solver.Best_Path (Num_Next,
                                           Next,
                                           Cost,
                                           Estimate,
                                           Image,
                                           Map.Path,
                                           Prepend);

   ---------------
   -- Best_Path --
   ---------------

   function Best_Path (This : Object;
                       Ini,
                       Fin : Location'Class) return Path_With_Cost
   is
      pragma Unreferenced (This);
      Result : Path_With_Cost;
   begin
      Best (Location_Handle.Set (Ini),
            Location_Handle.Set (Fin),
            Result.Path,
            Result.Cost);
      return Result;
   end Best_Path;

   ---------------
   -- Best_Path --
   ---------------

   function Best_Path (This : Object;
                       Ini,
                       Fin  : Types.Pose) return Path_With_Cost
   is
      Thix : Object'Class renames Object'Class (This);
   begin
      return Thix.Best_Path (Thix.Nearest_Location (Ini),
                             Thix.Nearest_Location (Fin));
   end Best_Path;

   ----------
   -- Hash --
   ----------

   function Hash (This : Object) return String is
   begin
      raise Program_Error with "Must be overriden if used";
      return "";
   end Hash;

   function Hash (This : Object'Class) return String is
   begin
      return This.Hash;
   end Hash;

   ----------------------
   -- Get_Cost_Between --
   ----------------------

   function Get_Cost_Between (This : Object;
                              Ini,
                              Fin  : Location'Class) return Costs
   is
      I : constant Ends_Costs_Maps.Cursor :=
            This.Cost_Cache.Find
              ((Location_Handle.Set (Ini), Location_Handle.Set (Fin)));
   begin
      if Ends_Costs_Maps.Has_Element (I) then
--           Log ("COST HIT", Debug, Detail_Section);
         return Ends_Costs_Maps.Element (I);
      else
         declare
            C : constant Costs := Ini.Real_Cost (Fin);
         begin
--              Log ("COST MISS", Debug, Detail_Section);
            This.Self.Cost_Cache.Insert
              ((Location_Handle.Set (Ini), Location_Handle.Set (Fin)), C);
            return C;
         end;
      end if;
   end Get_Cost_Between;

   ----------------------
   -- Get_Cost_Between --
   ----------------------

   function Get_Cost_Between
     (This : Object;
      Ini,
      Fin  : Types.Pose) return Costs
   is
      Thix : Object'Class renames Object'Class (This);
      use Types.Operations;
      Ini_Loc : constant Location'Class := Thix.Nearest_Location (Ini);
      Fin_Loc : constant Location'Class := Thix.Nearest_Location (Fin);
   begin
      if Ini_Loc = Fin_Loc then
         return Costs (Distance (Ini, Fin));
      elsif Ini_Loc.Is_Neighbor (Fin_Loc) then
         return Costs (Distance (Ini, Fin));
      else
         return Thix.Get_Cost_Between (Ini_Loc, Fin_Loc);
      end if;
   end Get_Cost_Between;

   --------------------------
   -- Get_Cost_Across_Path --
   --------------------------

   function Get_Cost_Across_Path
     (This    : Object;
      Ini,
      Fin     : Types.Pose;
      Route   : Path) return Costs
   is
      Thix : Object'Class renames Object'Class (This);
      pragma Unreferenced (This);

      use Paths,
          Types.Operations;
      R : Path  := Route;
   begin
      if
        not R.Is_Empty and then
        Thix.Nearest_Location (Ini) = Route.First_Element
      then
         R.Delete_First;
      end if;
      if
        not R.Is_Empty and then
        Thix.Nearest_Location (Fin) = Route.Last_Element
      then
         R.Delete_Last;
      end if;

      if R.Is_Empty then
         return Costs (Distance (Ini, Fin));
      else
         return
           Thix.Path_Cost (R) +
           Costs (Distance (Ini, Thix.Nearest_Pose (R.First_Element))) +
           Costs (Distance (Fin, Thix.Nearest_Pose (R.Last_Element)));
      end if;
   end Get_Cost_Across_Path;

   ---------------
   -- Path_Cost --
   ---------------

   function Path_Cost (This  : Object;
                       Route : Path) return Costs
   is
      pragma Unreferenced (This);
      use Paths;
      C : Costs := 0.0;
      procedure Path_Cost (I : Cursor) is
      begin
         if I /= Route.Last then
            C := C + Element (I).Real_Cost (Element (Next (I)));
         end if;
      end Path_Cost;
   begin
      Route.Iterate (Path_Cost'Access);
      return C;
   end Path_Cost;

   -------------------
   -- Compute_Costs --
   -------------------

   procedure Compute_Costs (This : in out Object) is
   begin
      raise Program_Error with "Must be overriden";
   end Compute_Costs;

   ----------------------
   -- Nearest_Location --
   ----------------------

   function Nearest_Location (This : Object;
                              Pose : Types.Pose) return Location'Class
   is
   begin
      raise Program_Error with "Must be overriden";
      pragma Warnings (Off);
      return Nearest_Location (This, Pose);
      pragma Warnings (On);
   end Nearest_Location;

   function Nearest_Location_To_Point (This : Object'Class;
                              P    : Types.Point) return Location'Class is
   begin
      return This.Nearest_Location (Pose => (P.X, P.Y, 0.0));
   end Nearest_Location_To_Point;

   ------------------
   -- Nearest_Pose --
   ------------------

   function Nearest_Pose (This : Object;
                          Loc  : Location'Class) return Types.Pose
   is
   begin
      raise Program_Error with "Must be overriden";
      return Types.Origin;
   end Nearest_Pose;

   -----------
   -- Steps --
   -----------

   function Steps (Ini,
                   Fin     : Location'Class;
                   Through : Path) return Natural
   is
      use Paths;
      I : Cursor  := First (Through);
      Y : Natural := 0;
   begin
      while Element (I) /= Ini loop
         Next (I);
      end loop;
      while Element (I) /= Fin loop
         Y := Y + 1;
         Next (I);
      end loop;
      return Y;
   end Steps;

   ----------
   -- Next --
   ----------

   function Next (Route : Path;
                  From  : Location'Class) return Location'Class
   is
      use Paths;
      I : Cursor := First (Route);
   begin
      while Has_Element (I) and then Element (I) /= From loop
         Next (I);
      end loop;
      if not Has_Element (I) then
         raise Constraint_Error with "Loc not in given route";
      elsif I = Last (Route) then
         return Route.Last_Element;
      else
         return Element (Next (I));
      end if;
   exception
      when others =>
         Log ("Not found: " & From.Image, Warning, Log_Section);
         Log ("Path was: " & Image (Route), Warning, Log_Section);
         raise;
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev (Route : Path;
                  From  : Location'Class) return Location'Class
   is
      use Paths;
      I : Cursor := Last (Route);
   begin
      while Has_Element (I) and then Element (I) /= From loop
         Previous (I);
      end loop;
      if not Has_Element (I) then
         raise Constraint_Error with "Loc not in given route";
      elsif I = First (Route) then
         return Route.First_Element;
      else
         return Element (Previous (I));
      end if;
   exception
      when others =>
         Log ("Not found: " & From.Image, Warning, Log_Section);
         Log ("Path was: " & Image (Route), Warning, Log_Section);
         raise;
   end Prev;

   ---------------
   -- Is_Before --
   ---------------

   function Is_Before (Route : Path;
                       Loc   : Location'Class;
                       Than  : Location'Class) return Boolean
   is
      use Paths;
      I     : Cursor  := Last (Route);
      Found : Boolean := False;
   begin
      while Has_Element (I) loop
         if Element (I) = Loc then
            return Found;
         end if;
         if Element (I) = Than then
            Found := True;
         end if;
         Previous (I);
      end loop;

      raise Constraint_Error with "Path doesn't contain target loc";
   end Is_Before;

   ---------------------
   -- Common_Ancestor --
   ---------------------

   function Common_Ancestor (X, Y : Path) return Location'Class is
      use Paths;
   begin
      return Element (Paths.Cursor'(Common_Ancestor (X, Y)));
   end Common_Ancestor;

   ---------------------
   -- Common_Ancestor --
   ---------------------

   function Common_Ancestor (X, Y : Path) return Paths.Cursor is
      use Paths;
   begin
      if X.Is_Empty or else Y.Is_Empty then
         raise Constraint_Error with "Empty path in Common_Ancestor";
      elsif X.First_Element /= Y.First_Element then
         raise Constraint_Error with "First location already differs";
      else
         declare
            I : Cursor := X.First;
            J : Cursor := Y.First;
         begin
            loop
               if
                 not Has_Element (Next (I)) or else
                 not Has_Element (Next (J)) or else
                 Element (Next (I)) /= Element (Next (J))
               then
                  return I;
               else
                  Next (I);
                  Next (J);
               end if;
            end loop;
         end;
      end if;
   end Common_Ancestor;

   --------------------------
   -- Last_Y_Location_In_X --
   --------------------------

   function Last_Y_Location_In_X (X, Y : Path) return Location'Class is
      use Paths;
      I : Cursor := Last (Y);
   begin
      while Has_Element (I) loop
         if X.Contains (Element (I)) then
            return Element (I);
         else
            Previous (I);
         end if;
      end loop;
      raise Constraint_Error with "Paths have no common locations";
   end Last_Y_Location_In_X;

   --------------
   -- To_Poses --
   --------------

   function To_Poses (This : Object;
                      Path : Map.Path)
                      return Sancta.Containers.Pose_Vectors.Vector
   is
      P : Sancta.Containers.Pose_Vectors.Vector;
      procedure To_Poses (I : Paths.Cursor) is
      begin
         P.Append (Object'Class (This).Nearest_Pose (Paths.Element (I)));
      end To_Poses;
   begin
      Path.Iterate (To_Poses'Access);
      return P;
   end To_Poses;

   -----------
   -- Image --
   -----------

   function Image (P : Path) return String is
      use Agpl.Ustrings, Agpl.Ustrings.ASU;
      R : UString;
      use Paths;
      procedure Image (I : Cursor) is
      begin
         Append (R, Element (I).Image);
      end Image;
   begin
      P.Iterate (Image'Access);
      return +R;
   end Image;

   ------------
   -- Prefix --
   ------------

   ------------
   -- Prefix --
   ------------

   function Prefix (Route : Path;
                    Loc   : Location'Class) return Path
   is
      Result : Path;
      use Paths;
      I : Cursor := First (Route);
   begin
      while Has_Element (I) and then Element (I) /= Loc loop
         Result.Append (Element (I));
         Next (I);
      end loop;
      return Result;
   end Prefix;

   ------------------------------
   -- Nearest_Location_In_Path --
   ------------------------------

   function Nearest_Location_In_Path (M    : Object;
                                      To   : Location'Class;
                                      From : Path) return Location'Class
   is
      pragma Unreferenced (M);

      Best_Cost : Costs := Infinite;
      Best_Loc  : Location_Handle.Object;

      use Paths;
      procedure Check (I : Cursor) is
      --           Best : constant Path_With_Cost := M.Best_Path (Ini => Element (I),
      --                                                          Fin  => To);
         Best : constant Costs := Element (I).Real_Cost (To);
      begin
         if Best < Best_Cost then
            Best_Cost := Best;
            Best_Loc.Set (Element (I));
         end if;
      end Check;
   begin
      From.Iterate (Check'Access);
      return Best_Loc.Get;
   end Nearest_Location_In_Path;

   --------------------------------------------
   -- Nearest_Location_In_Path_From_Path_End --
   --------------------------------------------

   function Nearest_Location_In_Path_From_Path_End
     (M    : Object;
      To   : Location'Class;
      From : Path) return Location'Class
   is
      Best_Cost : Costs := Infinite;
      Best_Loc  : Location_Handle.Object;

      use Paths;
      procedure Check (I : Cursor) is
         Best : constant Path_With_Cost := M.Best_Path (Ini => Element (I),
                                                        Fin  => To);
         Back : constant Costs :=
           M.Path_Cost
             (Path_Fields.Slice (From, Element (I), From.Last_Element));
      begin
         if Best.Cost + Back < Best_Cost then
            Best_Cost := Best.Cost + Back;
            Best_Loc.Set (Element (I));
         end if;
      end Check;
   begin
      From.Iterate (Check'Access);
      return Best_Loc.Get;
   end Nearest_Location_In_Path_From_Path_End;

   ----------------------------------------
   -- Nearest_Location_In_Path_Euclidean --
   ----------------------------------------

   function Nearest_Location_In_Path_Estimated
     (M    : Object;
      To   : Location'Class;
      From : Path) return Location'Class
   is
      pragma Unreferenced (M);

--        Timer : Agpl.Chronos.Object;

      Best_Cost : Costs := Infinite;
      Best_Loc  : Location_Handle.Object;

      use Paths;
      procedure Check (I : Cursor) is

         Best : constant Costs := Element (I).Estimate_Cost (To);
      begin
         if Best < Best_Cost then
            Best_Cost := Best;
            Best_Loc.Set (Element (I));
         end if;
      end Check;
   begin
      From.Iterate (Check'Access);
      --  Log ("WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW " & Timer.Image, Always);
      return Best_Loc.Get;
   end Nearest_Location_In_Path_Estimated;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Endpoints) return Boolean is
   begin
      return
        L.Ini.Ref.all < R.Ini.Ref.all or else
        (L.Ini.Ref.all = R.Ini.Ref.all and then L.Fin.Ref.all < R.Fin.Ref.all);
   end "<";

   ----------
   -- Tail --
   ----------

   function Tail (Route : Path;
                  Loc   : Location'Class) return Path
   is
   begin
      return Path_Fields.Tail (Route, Loc);
   end Tail;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (L, R : Location'Class) return Boolean is
   begin
      return L."=" (R);
   end Is_Equal;

   -----------
   -- Write --
   -----------

   procedure Class_Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                          This   : in     Object'Class)
   is
   begin
      Log ("At sancta.map.write", Always, Log_Section);
      Write (Stream, This);
   end Class_Write;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
      use Loc_Obs_Maps;
      procedure Print (I : Cursor) is
      begin
         Log (Key (I).Image, Always, Log_Section);
      end Print;
   begin
      This.Loc_Obs.Iterate (Print'Access);
   end Print;

   ---------------------
   -- Class_Less_Than --
   ---------------------

   function Class_Less_Than (L, R : Relative_Position'Class) return Boolean is
   begin
      return L < R;
   end Class_Less_Than;

   -----------------
   -- Class_Image --
   -----------------

   function Class_Image (This : Relative_Position'Class) return String is
   begin
      return This.Image;
   end Class_Image;

   -----------
   -- Depth --
   -----------

   function Depth (L : Location'Class; P : Path) return Positive is
      use Paths;
      D : Positive := 1;
      I : Cursor := P.First;
   begin
      while Has_Element (I) loop
         if Element (I) = L then
            return D;
         else
            D := D + 1;
            Next (I);
         end if;
      end loop;
      raise Constraint_Error with "Location not in given path";
   end Depth;

end Sancta.Map;
