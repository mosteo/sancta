with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Unchecked_Conversion;

with Agpl.Conversions;
with Agpl.Constants;

with Sancta.Types.Operations;

package body Sancta.Map.Qtree is

   package Id_Map_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Object_Access);

   Global_Maps : Id_Map_Maps.Map;
   --  Used for serialization of locations

   -------------
   -- Get_Map --
   -------------

   function Get_Map (This : Location) return access Map.Object'Class is
   begin
      return This.Cell.Qmap;
   end Get_Map;

   -------------------
   -- Set_Cell_Size --
   -------------------

   procedure Set_Cell_Size
     (This : in out Object;
      Min,
      Max  :        Real)
   is
   begin
      This.Cell_Min := Min;
      This.Cell_Max := Max;
   end Set_Cell_Size;

   -----------------
   -- Set_Options --
   -----------------

   procedure Set_Options (This : in out Object;
                          Opts :        Extra_Options)
   is
   begin
      This.Opts := Opts;
   end Set_Options;

   ---------
   -- "=" --
   ---------

   function "="
     (L, R : Location)
      return Boolean
   is
   begin
      return L.Cell = R.Cell;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<"
     (L, R : Location)
      return Boolean
   is
      --  Since this is only used for storage, we take the quick way:
      function Ptr is new Ada.Unchecked_Conversion (Cell_Access, Integer);
   begin
      return Ptr (L.Cell) < Ptr (R.Cell);
   end "<";

   -----------
   -- Image --
   -----------

   function Image
     (This : Location)
      return String
   is
      use Agpl.Conversions;
      C : Cell_Type renames This.Cell.all;
   begin
      return
      "D:" & This.Cell.Depth'Img &
      " T:" & This.Cell.Terr'Img &
      " P:" & This.Cell.Index'Img &
      " C:" & This.Cell.Has_Children'Img &
      " N:" & This.Cell.Neighbors.Length'Img &
      " (" & To_String (Float (C.Xl)) & "," & To_String (Float (C.Yb)) & ")-(" &
      To_String (Float (C.Xr)) & "," & To_String (Float (C.Yt)) & ")";
   end Image;

   -------------------
   -- Num_Neighbors --
   -------------------

   function Num_Neighbors
     (L : Location)
      return Natural
   is
   begin
      return Natural (L.Cell.Neighbors.Length);
   end Num_Neighbors;

   --------------
   -- Neighbor --
   --------------

   function Neighbor
     (L : Location;
      I : Positive)
      return Location
   is
   begin
      return (Cell => L.Cell.Neighbors.Element (I));
   end Neighbor;

   -----------------
   -- Is_Neighbor --
   -----------------

   function Is_Neighbor
     (L, R : Location)
      return Boolean
   is
   begin
      return Is_Neighbor (L.Cell, R.Cell);
   end Is_Neighbor;

   -----------------
   -- Is_Neighbor --
   -----------------

   function Is_Neighbor (C1, C2 : not null access Cell_Type) return Boolean is
   begin
      pragma Assert (C1.Xl < C1.Xr and then C1.Yb < C1.Yt);
      pragma Assert (C2.Xl < C2.Xr and then C2.Yb < C2.Yt);

      if (C1.Xr = C2.Xl or else C1.Xl = C2.Xr) and then
        ((C1.Yt >= C2.Yt and then C1.Yb <= C2.Yb) or else
         (C2.Yt >= C1.Yt and then C2.Yb <= C1.Yb))
      then
         return True;
      end if;

      if (C1.Yt = C2.Yb or else C1.Yb = C2.Yt) and then
        ((C1.Xr >= C2.Xr and then C1.Xl <= C2.Xl) or else
         (C2.Xr >= C1.Xr and then C2.Xl <= C1.Xl))
      then
         return True;
      end if;

      return False;
   end Is_Neighbor;

   ---------------
   -- Real_Cost --
   ---------------

   function Real_Cost
     (Ini, Fin : Location)
      return Costs
   is
      use Sancta.Types.Operations;
   begin
      if Ini.Is_Neighbor (Fin) then
--           Log ("Returning adjacent cost", Always);
         return
           Costs (Distance (Ini.Cell.Qmap.Nearest_Pose (Ini),
                            Fin.Cell.Qmap.Nearest_Pose (Fin)));
      else
--           Log ("Returning computed cost", Always);
         return Ini.Cell.Qmap.Best_Path (Ini, Fin).Cost;
      end if;
   end Real_Cost;

   -------------------
   -- Estimate_Cost --
   -------------------

   function Estimate_Cost
     (Ini, Fin : Location)
      return Costs
   is
      use Sancta.Types.Operations;
   begin
      return Costs (Distance (Ini.Cell.Qmap.Nearest_Pose (Ini),
                              Fin.Cell.Qmap.Nearest_Pose (Fin)));
   end Estimate_Cost;

   --------------
   -- Position --
   --------------

   function Position
     (Here, There : Location)
      return Map.Relative_Position'Class
   is
      use Neigh_Vectors;
      I : constant Cursor := Here.Cell.Neighbors.Find (There.Cell);
   begin
      if Has_Element (I) then
         return Relative_Position'(Pos => To_Index (I));
      else
         raise Constraint_Error with "Locations aren't neighbors";
      end if;
   end Position;

   ----------------------
   -- Nearest_Location --
   ----------------------

   function Nearest_Location
     (This : Object;
      Pose : Types.Pose)
      return Map.Location'Class
   is
   begin
      return Location'(Cell => This.Root.Nearest_Cell (Pose));
   end Nearest_Location;

   ------------------
   -- Nearest_Pose --
   ------------------

   function Nearest_Pose
     (This : Object;
      Loc  : Map.Location'Class)
      return Types.Pose
   is
      pragma Unreferenced (This);
      C : Cell_Type renames Location (Loc).Cell.all;
   begin
      return
        (X => Real ((C.Xl + C.Xr) / 2.0),
         Y => Real ((C.Yb + C.Yt) / 2.0),
         A => 0.0);
   end Nearest_Pose;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children (Cell : not null access Cell_Type) return Boolean is
   begin
      for I in Cell.Children'Range loop
         if Cell.Children (I) /= null then
            return True;
         end if;
      end loop;
      return False;
   end Has_Children;

   ------------------
   -- Nearest_Cell --
   ------------------

   function Nearest_Cell (Cell : not null access Cell_Type;
                          Pose :                 Types.Pose) return Cell_Access
   is
      function S is new Agpl.Conversions.To_Str (X_Real);
      function S is new Agpl.Conversions.To_Str (Y_Real);
   begin
      if not Cell.Has_Children then
         return Cell_Access (Cell);
      else
         for I in Cell.Children'Range loop
            if Cell.Children (I).Contains (Pose) then
               return Nearest_Cell (Cell.Children (I), Pose);
            end if;
         end loop;
         Log ("Map bounds: X " &
              S (Cell.Qmap.Root.Xl) & " to " & S (Cell.Qmap.Root.Xr) &
              "; Y " & S (Cell.Qmap.Root.Yb) & " to " & S (Cell.Qmap.Root.Yt),
              Error, Log_Section);
         Log ("Pose requested: " & Image (Pose), Error, Log_Section);
         raise Constraint_Error
           with "Pose is outside map or bad map: " & Image (Pose);
      end if;
   end Nearest_Cell;

   --------------
   -- Contains --
   --------------

   function Contains (Cell : not null access Cell_Type;
                      Pose :                 Types.Pose) return Boolean
   is
   begin
      return
        Real (Cell.Xl) <= Pose.X and then
        Real (Cell.Xr) > Pose.X and then
        Real (Cell.Yb) <= Pose.Y and then
        Real (Cell.Yt) > Pose.Y;
   end Contains;

   Neighbors_Found : Natural := 0;

   ------------
   -- Create --
   ------------

   procedure Create
     (This : in out Object;
      Xmin,
      Xmax : X_Real;
      Ymin,
      Ymax : Y_Real;
      Id   : String := Default_Map_Id)
   is
      --  This is all to have a square top cell, centered on the given coords.
      Xsize : constant X_Real := Xmax - Xmin;
      Ysize : constant Y_Real := Ymax - Ymin;
      Size  : constant Real   := Real'Max (Real (Xsize), Real (Ysize));
      Xl    : constant X_Real := Xmin - (X_Real (Size) - Xsize) / 2.0;
      Xr    : constant X_Real := Xmax + (X_Real (Size) - Xsize) / 2.0;
      Yb    : constant Y_Real := Ymin - (Y_Real (Size) - Ysize) / 2.0;
      Yt    : constant Y_Real := Ymax + (Y_Real (Size) - Ysize) / 2.0;

      procedure Split is new Qtree.Split (Terrain);
   begin
      This.Root := new Cell_Type'(Qmap  => This.Qself,
                                  Terr  => Terrain (Xl, Xr, Yb, Yt),
                                  Depth => 1,
                                  Index => Child_Index'First,
                                  Xl    => Xl,
                                  Xr    => Xr,
                                  Yb    => Yb,
                                  Yt    => Yt,
                                  Parent    => null,
                                  Children  => <>,
                                  Neighbors => <>);
      This.Set_At (This.Root.Loc, This.Root.Obs);

      if True or else not This.Opts.Lazy_Split then
         Log ("Create: Splitting cells...", Informative, Log_Section);
         Split (This, This.Root, Recursive => True);

         Log ("Create: Cells created:" & This.Loc_Obs.Length'Img,
              Informative, Log_Section);

         Log ("[After-Split] Neighbors found:" & Neighbors_Found'Img,
              Informative, Log_Section);
      end if;

      Log ("Create: Map loaded with" & This.Loc_Obs.Length'Img & " cells.",
           Informative, Log_Section);

      --  Keep it for serialization
      This.Id := +Id;
      Global_Maps.Insert (Id, This'Unchecked_Access);
   end Create;



   ---------
   -- Loc --
   ---------

   function Loc (Cell : not null access Cell_Type) return Location'Class is
   begin
      return Location'(Cell => Cell_Access (Cell));
   end Loc;

   ---------
   -- Obs --
   ---------

   function Obs (Cell : not null access Cell_Type) return Observation'Class is
   begin
      return Observation'(Terrain => Cell.Terr);
   end Obs;

   -----------
   -- Split --
   -----------

   procedure Split (This      :          in out Object;
                    Cell      : not null access Cell_Type'Class;
                    Recursive : Boolean := True)
   is
      M : Object renames This;
   begin
      --  Log ("Split: at depth" & Cell.Depth'Img, Debug, Det_Section);
      if Cell.Terr /= Mixed and then
        Cell.Xr - Cell.Xl < X_Real (M.Cell_Max) and then
        Cell.Yt - Cell.Yb < Y_Real (M.Cell_Max)
      then
         if Cell.Terr /= Free then
            Cell.Remove_From_Neighbors;
         end if;
         Log ("Split: rec-stop homogeneous", Debug, Det_Section);
         return; -- No need to split homogeneous cell
      end if;

      if (Cell.Xr - Cell.Xl) / 2.0 < X_Real (M.Cell_Min) or else
         (Cell.Yt - Cell.Yb) / 2.0 < Y_Real (M.Cell_Min)
      then
         if Cell.Terr /= Free then
            Cell.Remove_From_Neighbors;
         end if;
         Log ("Split: rec-stop small", Debug, Det_Section);
         return; -- No need to split small enough cell
      end if;

      --  Split proper starts here

      declare
         Xm : constant X_Real := (Cell.Xl + Cell.Xr) / 2.0;
         Ym : constant Y_Real := (Cell.Yb + Cell.Yt) / 2.0;
      begin
         --  Top-left
         Cell.Children (No) := new Cell_Type'
           (Cell.Qmap,
            Terrain (Cell.Xl, Xm, Ym, Cell.Yt),
            Cell.Depth + 1,
            No,
            Cell.Xl, Xm, Ym, Cell.Yt,
            Cell_Access (Cell),
            Children  => <>,
            Neighbors => <>);

         --  Top-right
         Cell.Children (Ne) := new Cell_Type'
           (Cell.Qmap,
            Terrain (Xm, Cell.Xr, Ym, Cell.Yt),
            Cell.Depth + 1,
            Ne,
            Xm, Cell.Xr, Ym, Cell.Yt,
            Cell_Access (Cell),
            Children  => <>,
            Neighbors => <>);

         --  Bottom-right
         Cell.Children (Se) := new Cell_Type'
           (Cell.Qmap,
            Terrain (Xm, Cell.Xr, Cell.Yb, Ym),
            Cell.Depth + 1,
            Se,
            Xm, Cell.Xr, Cell.Yb, Ym,
            Cell_Access (Cell),
            Children  => <>,
            Neighbors => <>);

         --  Bottom-left
         Cell.Children (So) := new Cell_Type'
           (Cell.Qmap,
            Terrain (Cell.Xl, Xm, Cell.Yb, Ym),
            Cell.Depth + 1,
            So,
            Cell.Xl, Xm, Cell.Yb, Ym,
            Cell_Access (Cell),
            Children  => <>,
            Neighbors => <>);

      end;

      --  Neighborhoodkeeping
      Test_And_Make_Neighbors (Cell.Children (No), Cell.Children (Ne), Mixed);
      Test_And_Make_Neighbors (Cell.Children (Se), Cell.Children (Ne), Mixed);
      Test_And_Make_Neighbors (Cell.Children (Se), Cell.Children (So), Mixed);
      Test_And_Make_Neighbors (Cell.Children (No), Cell.Children (So), Mixed);
      for I in Cell.Children'Range loop
         for J in Cell.Neighbors.First_Index .. Cell.Neighbors.Last_Index loop
            Test_And_Make_Neighbors (Cell.Children (I),
                                     Cell.Neighbors.Element (J), Mixed);
         end loop;
      end loop;
      Cell.Remove_From_Neighbors;

      --  De-de-de-de-de-de-deeeeper! (Dr'Dick)
      for I in Cell.Children'Range loop
         M.Set_At (Cell.Children (I).Loc, Cell.Children (I).Obs);
         if True or else Recursive then
            Split (This, Cell.Children (I), Recursive);
         end if;
      end loop;
   end Split;

   --------------------
   -- Make_Neighbors --
   --------------------

   procedure Make_Neighbors (C, D : not null access Cell_Type) is
   begin
      Neighbors_Found := Neighbors_Found + 1;
      if Neighbors_Found rem 500 = 0 then
         Log ("Merging step:" & Neighbors_Found'Img, Debug, Log_Section);
      end if;

      C.Neighbors.Append (Cell_Access (D));
      D.Neighbors.Append (Cell_Access (C));
   end Make_Neighbors;

   -----------------------------
   -- Test_And_Make_Neighbors --
   -----------------------------

   procedure Test_And_Make_Neighbors (C, D  : not null access Cell_Type;
                                      Valid : Terrains := Free) is
   begin
      if C /= D and then
        not C.Has_Children and then
        not D.Has_Children and then
        C.Terr <= Valid and then
        D.Terr <= Valid and then
        Is_Neighbor (D, C) and then
        not C.Neighbors.Contains (Cell_Access (D))
      then
         Make_Neighbors (C, D);
      end if;
   end Test_And_Make_Neighbors;

   ----------------------------
   -- Merge_Neighbors_Bubble --
   ----------------------------
   --  Obsolete, slower way
   procedure Merge_Neighbors_Bubble (This : in out Object) is
      use Sancta.Map.Loc_Obs_Maps;
      I     : Cursor  := This.Loc_Obs.First;
   begin
      while Has_Element (I) loop
         declare
            C : constant Cell_Access := Location (Key (I)).Cell;
            J : Cursor := Next (I);
         begin
            while Has_Element (J) loop
               declare
                  D : constant Cell_Access := Location (Key (J)).Cell;
               begin
                  Test_And_Make_Neighbors (C, D);
               end;
               Next (J);
            end loop;
         end;
         Next (I);
      end loop;

      Log ("[Bubble] Neighbors found:" & Neighbors_Found'Img,
           Debug, Log_Section);
   end Merge_Neighbors_Bubble;

   -------------------------------
   -- Merge_Neighbors_Recursive --
   -------------------------------

   procedure Merge_Neighbors_Recursive (This : in out Object) is
   begin
      if This.Root.Has_Children then
         This.Root.Merge_Siblings;
      end if;
      Log ("[Recursive] Neighbors found:" & Neighbors_Found'Img,
           Debug, Log_Section);
   end Merge_Neighbors_Recursive;

   procedure Merge_Neighbors (This : in out Object)
     renames Merge_Neighbors_Recursive;

   --------------------
   -- Merge_Siblings --
   --------------------

   procedure Merge_Siblings (Cell : not null access Cell_Type) is
      procedure Merge_Siblings (Leaf : not null access Cell_Type;
                                Sibl : not null access Cell_Type)
      is
      begin
         if Leaf /= Sibl then
            if Sibl.Has_Children then
               for I in Sibl.Children'Range loop
                  Merge_Siblings (Cell, Sibl.Children (I));
               end loop;
            else
               Test_And_Make_Neighbors (Cell, Sibl);
            end if;
         end if;
      end Merge_Siblings;
   begin
      if Cell.Has_Children then
         --  Go deeper
         for I in Cell.Children'Range loop
            Cell.Children (I).Merge_Siblings;
         end loop;
      else
         Merge_Siblings (Cell, Cell.Qmap.Root);
      end if;
   end Merge_Siblings;

---------------------------
-- Remove_From_Neighbors --
---------------------------

   procedure Remove_From_Neighbors (Cell : not null access Cell_Type) is
      use Neigh_Vectors;
   begin
      for I in reverse Cell.Neighbors.First_Index .. Cell.Neighbors.Last_Index loop
         declare
            F : Cursor :=
                  Cell.Neighbors.Element (I).Neighbors.Find (Cell_Access (Cell));
         begin
            if Has_Element (F) then
               Cell.Neighbors.Element (I).all.Neighbors.Delete (F);
               Neighbors_Found := Neighbors_Found - 1;
            else
               raise Program_Error with "Broken neighborhood!";
            end if;
         end;
      end loop;
      Cell.Neighbors.Clear;
   end Remove_From_Neighbors;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Relative_Position) return Boolean is
   begin
      return L.Pos < R.Pos;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (This : Relative_Position) return String is
   begin
      return This.Pos'Img;
   end Image;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class) is
   begin
      case This.Opts.Draw_Mode is
         when Skel =>
            This.Draw_Skel (D);
         when Skel_With_Neighbors =>
            This.Draw_Skel_With_Neighbors (D);
         when Solid =>
            This.Draw_Solid (D);
         when Solid_With_Skel =>
            This.Draw_Solid (D);
            This.Draw_Skel (D);
      end case;
   end Draw;

   ---------------
   -- Draw_Skel --
   ---------------

   procedure Draw_Skel
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Constants;
      use Map.Loc_Obs_Maps;
      procedure Draw (I : Cursor) is
         Loc  : constant Location := Location (Key (I));
         Cell : Cell_Access renames Loc.Cell;
      begin
         if Cell.Parent = null then -- Root, bounding box
            D.Set_Color (Yellow, Alpha_Opaque);
--              D.Draw_Rectangle
--                (Float (Cell.Xl), Float (Cell.Yb),
--                 Float (Cell.Xr), Float (Cell.Yt));
         elsif Cell.Terr = Free and then not Cell.Has_Children then
            D.Set_Color (Soft_Green, Alpha_Opaque);
            D.Draw_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));
         end if;
      end Draw;
   begin
      This.Loc_Obs.Iterate (Draw'Access);
      --  Map Boundary
      D.Set_Color (Black, Alpha_Opaque);
      D.Draw_Rectangle (Float (This.Root.Xl), Float (This.Root.Yb),
                        Float (This.Root.Xr), Float (This.Root.Yt));
   end Draw_Skel;

   ----------------
   -- Draw_Solid --
   ----------------

   procedure Draw_Solid
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Constants;
      use Map.Loc_Obs_Maps;
      procedure Draw (I : Cursor) is
         Loc  : constant Location := Location (Key (I));
         Cell : Cell_Access renames Loc.Cell;
      begin
         if Cell.Terr = Obstacle and then not Cell.Has_Children then
            D.Set_Color (Black, Alpha_Opaque);
            D.Fill_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));
         elsif Cell.Terr = Mixed and then not Cell.Has_Children then
            D.Set_Color (Gray, Alpha_Opaque);
            D.Fill_Rectangle
              (Float (Cell.Xl), Float (Cell.Yb),
               Float (Cell.Xr), Float (Cell.Yt));
         elsif Cell.Terr = Free and then not Cell.Has_Children then
            null;
--              D.Set_Color (White, Alpha_Opaque);
--              D.Fill_Rectangle
--                (Float (Cell.Xl), Float (Cell.Yb),
--                 Float (Cell.Xr), Float (Cell.Yt));
         end if;
      end Draw;
   begin
      This.Loc_Obs.Iterate (Draw'Access);
      --  Map Boundary
      D.Set_Color (Black, Alpha_Opaque);
      D.Draw_Rectangle (Float (This.Root.Xl), Float (This.Root.Yb),
                        Float (This.Root.Xr), Float (This.Root.Yt));
   end Draw_Solid;

   ------------------------------
   -- Draw_Skel_With_Neighbors --
   ------------------------------

   procedure Draw_Skel_With_Neighbors
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Constants;
      M : Object renames This;

      Terr_Color : constant array (Terrains) of Agpl.Types.Rgb_triplet :=
                     (Free => White, Obstacle => Black, Mixed => Blue);

      procedure Draw_Cell (Cell : not null access Cell_Type) is
      begin
         if Cell.Has_Children then
            for I in Cell.Children'Range loop
               Draw_Cell (Cell.Children (I));
            end loop;
            return;
         end if;

         --  Background
         D.Set_Color (Terr_Color (Cell.Terr), Alpha_Opaque);
         D.Draw_rectangle
           (Float (Cell.Xl), Float (Cell.Yb),
            Float (Cell.Xr), Float (Cell.Yt));
         if Cell.Terr /= Free then
            D.Draw_Line
              (Float (Cell.Xl), Float (Cell.Yt),
               Float (Cell.Xr), Float (Cell.Yb));
            D.Draw_Line
              (Float (Cell.Xr), Float (Cell.Yt),
               Float (Cell.Xl), Float (Cell.Yb));
         end if;

         --  Nears
         for I in Cell.Neighbors.First_Index .. Cell.Neighbors.Last_Index loop
            declare
               P1 : constant Pose := Cell.Qmap.Nearest_Pose (Cell.Loc);
               P2 : constant Pose := Cell.Qmap.Nearest_Pose
                 (Cell.Neighbors.Element (I).Loc);
            begin
               D.Set_Color (Green, Alpha_Opaque);
               D.Draw_Line
                 (Float (P1.X), Float (P1.Y),
                  Float (P2.X), Float (P2.Y));
            end;
         end loop;

         --  Boundary
         D.Set_Color (Yellow, Alpha_Opaque);
         D.Draw_Rectangle (Float (Cell.Xl), Float (Cell.Yb),
                           Float (Cell.Xr), Float (Cell.Yt));
      end Draw_Cell;

   begin
      --  Cells
      Draw_Cell (This.Root);

      --  Map Boundary
      D.Set_Color (Black, Alpha_Opaque);
      D.Draw_Rectangle (Float (M.Root.Xl), Float (M.Root.Yb),
                        Float (M.Root.Xr), Float (M.Root.Yt));
   end Draw_Skel_With_Neighbors;

   --------------------
   -- Is_Traversable --
   --------------------

   function Is_Traversable (This : Observation) return Boolean is
   begin
      return This.Terrain = Free;
   end Is_Traversable;

   --------------------
   -- Is_Traversable --
   --------------------

   function Is_Traversable (This : Map.Observation'Class) return Boolean is
   begin
      return Observation (This).Is_Traversable;
   end Is_Traversable;

   ---------------
   -- Best_Path --
   ---------------

   function Best_Path (This : Object;
                       Ini,
                       Fin  : Map.Location'Class) return Path_With_Cost
   is
   begin
      return Map.Object (This).Best_Path (Ini, Fin);
   end Best_Path;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Location;
                   D    : in out Agpl.Drawing.Drawer'Class)
   is
   begin
      D.Draw_Rectangle (Float (This.Cell.Xl + 0.1), Float (This.Cell.Yb + 0.1),
                        Float (This.Cell.Xr - 0.1), Float (This.Cell.Yt - 0.1));
   end Draw;

   ----------
   -- Read --
   ----------

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Location)
   is
      Id : constant String := String'Input (Stream);
      P  :          Types.Pose;
   begin
      Types.Pose'Read (Stream, P);
      if Global_Maps.Contains (Id) then
         This := Location (Global_Maps.Element (Id).Nearest_Location (P));
      else
         raise Unknown_Map;
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Location)
   is
   begin
      String'Output (Stream, +This.Cell.Qmap.Id);
      Types.Pose'Write
        (Stream,
         This.Cell.Qmap.Nearest_Pose (This));
   end Write;

   ------------
   -- Coords --
   ------------

   function Coords (L : Location) return Cell_Coords is
   begin
      return (Xl => L.Cell.Xl,
              Xr => L.Cell.Xr,
              Yb => L.Cell.Yb,
              Yt => L.Cell.Yt);
   end Coords;

end Sancta.Map.Qtree;
