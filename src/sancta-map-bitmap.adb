with Sancta.Map.Create_Wavefront;
with Sancta.Types.Operations;

with Agpl.Constants;
with Agpl.Strings; use Agpl.Strings;
with Agpl.Text_Io; use Agpl.Text_Io;

with Png_Io;

with Gnat.Md5;

with Ada.Unchecked_Deallocation;

-----------------------
-- Sancta.Map.Bitmap --
-----------------------

package body Sancta.Map.Bitmap is

   procedure Free is new Ada.Unchecked_Deallocation (Matrix,
                                                     Matrix_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Cost_Matrix,
                                                     Cost_Matrix_Access);

   -------------
   -- Get_Map --
   -------------

   function Get_Map (L : Bit_Location) return access Map.Object'Class is
   begin
      return L.M;
   end Get_Map;

   ---------
   -- "=" --
   ---------

   function "=" (X, Y : Bit_Location) return Boolean is
   begin
--      Log ("bitloc.= called", Always);
      return X.X = Y.X and then X.Y = Y.Y;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (X, Y : Bit_Location) return Boolean is
   begin
      return X.X < Y.X or else (X.X = Y.X and then X.Y < Y.Y);
   end "<";

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (This : in out Object;
                       Xmin :        Abscissa;
                       Ymin :        Ordinate;
                       Xmax :        Abscissa;
                       Ymax :        Ordinate)
   is
   begin
      Free (This.Data);

      This.Data := new Matrix (Ymin .. Ymax, Xmin .. Xmax);
   end Set_Size;

   ---------------
   -- Get_X_Min --
   ---------------

   function Get_X_Min (This : Object) return Abscissa is
   begin
      return This.Data'First (2);
   end Get_X_Min;

   ---------------
   -- Get_X_Max --
   ---------------

   function Get_X_Max (This : Object) return Abscissa is
   begin
      return This.Data'Last (2);
   end Get_X_Max;

   ---------------
   -- Get_Y_Min --
   ---------------

   function Get_Y_Min (This : Object) return Ordinate is
   begin
      return This.Data'First (1);
   end Get_Y_Min;

   ---------------
   -- Get_Y_Max --
   ---------------

   function Get_Y_Max (This : Object) return Ordinate is
   begin
        return This.Data'Last (1);
   end Get_Y_Max;

   --------------
   -- Is_Known --
   --------------

   function Is_Known
     (This : Object;
      Loc  : Location'Class)
      return Boolean
   is
      pragma Unreferenced (This, Loc);
   begin
      return True;
   end Is_Known;

   --------------
   -- Is_Known --
   --------------

   function Is_Known
     (This : Object;
      X    : Abscissa;
      Y    : Ordinate)
      return Boolean
   is
      pragma Unreferenced (This, X, Y);
   begin
      return True;
   end Is_Known;

   ------------
   -- Get_At --
   ------------

   function Get_At
     (This : Object;
      Loc  : Location'Class)
      return Observation'Class
   is
      L : Bit_Location renames Bit_Location (Loc);
   begin
      return Bit_Observation'(Bit => This.Data (L.Y, L.X));
   end Get_At;

   ------------
   -- Get_At --
   ------------

   function Get_At
     (This : Object;
      X    : Abscissa;
      Y    : Ordinate)
      return Terrains
   is
   begin
      return This.Data (Y, X);
   end Get_At;

   ------------
   -- Set_At --
   ------------

   procedure Set_At
     (This : in out Object;
      Loc  : in     Location'Class;
      Obs  : in     Observation'Class)
   is
      L : Bit_Location    renames Bit_Location (Loc);
      O : Bit_Observation renames Bit_Observation (Obs);
   begin
      This.Set_At (L.X, L.Y, O.Bit);
   end Set_At;

   ------------
   -- Set_At --
   ------------
   Bug_Stage_Warned : Boolean := False;
   procedure Set_At
     (This : in out Object;
      X    :        Abscissa;
      Y    :        Ordinate;
      Bit  :        Terrains)
   is
   begin
      if Buggy_Stage_Png and then Y = -1 then
         --  Discard but warn!
         if not Bug_Stage_Warned then
            Bug_Stage_Warned := True;
            Log ("Bitmap.Set_At: Discarding -1 row because of stage bug!",
                 Warning, Log_Section);
         end if;
      else
         This.Data (Y, X) := Bit;
      end if;
   end Set_At;

   ----------
   -- Fill --
   ----------

   procedure Fill (This : in out Object;
                   Xmin :        Abscissa;
                   Ymin :        Ordinate;
                   Xmax :        Abscissa;
                   Ymax :        Ordinate;
                   Bit  :        Terrains)
   is
   begin
      for R in Ordinate'Max (This.Get_Y_Min, Ymin) ..
               Ordinate'Min (This.Get_Y_Max, Ymax) loop
         for C in Abscissa'Max (This.Get_X_Min, Xmin) ..
                  Abscissa'Min (This.Get_X_Max, Xmax)
         loop
            This.Set_At (C, R, Obstacle);
         end loop;
      end loop;
   end Fill;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object;
                  Full :        Matrix)
   is
   begin
      This.Finalize;
      This.Data := new Matrix'(Full);
   end Set;

   -----------
   -- Image --
   -----------

   function Image (This : Bit_Location) return String is
   begin
      return "(" & This.X'Img & "," & This.Y'Img & ")";
   end Image;

   ------------
   -- Adjust --
   ------------

   procedure Adjust     (This : in out Object) is
   begin
      if This.Data /= null then
         This.Data := new Matrix'(This.Data.all);
      end if;

      if This.Cost /= null then
         This.Cost := new Cost_Matrix'(This.Cost.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (This : in out Object) is
   begin
      Free (This.Data);
   end Finalize;

   --------------
   -- From_Png --
   --------------

   procedure From_Png (This : in out Object;
                       File :        String;
                       Load :        Loader)
   is
      use Png_Io;
      Png : Png_File;
   begin
      begin
         Log ("Opening: " & File, Informative, Log_Section);
         Open (Png, File);
         Log ("PNG BPP:"     & Sample_Depth (Png)'Img, Informative, Log_Section);
         Log ("PNG Is_RGB: " & Standard_Rgb (Png)'Img, Informative, Log_Section);
         if Buggy_Stage_Png then
            This.Set_Size (0, 0,
                           Abscissa (Width (Png)) - 1,
                           Ordinate (Height (Png)) - 2);
         else
            This.Set_Size (0, 0,
                           Abscissa (Width (Png)) - 1,
                           Ordinate (Height (Png)) - 1);
         end if;
         Close (Png);
      end;

      --  Non-dispatch to parent function
      Sancta.Map.Object (This).From_Png (File, Load);
   end From_Png;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
   begin
      for Row in reverse This.Data'Range loop
         Put ("+");
         for Col in This.Data'Range (2) loop
            if This.Data (Row, Col) = Obstacle then
               Put ("*");
            else
               Put (" ");
            end if;
         end loop;
         Put_Line ("+");
      end loop;
   end Print;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (This : Object) return access constant Matrix is
   begin
      return This.Data;
   end Get_Data;

   -------------------------
   -- Get_Closest_Free_To --
   -------------------------

   function Get_Closest_Free_To (This : Object;
                                 Loc  : Bit_Location'Class;
                                 Near : Neighbor_Function)
                                 return Bit_Location'Class
   is
      Pending : Location_Lists.List;
      Visited : Location_Sets.Set;
      use Location_Lists;
   begin
      Pending.Append (Loc);

      while not Pending.Is_Empty loop
         declare
            L : constant Bit_Location := Bit_Location (Pending.First_Element);
            N : constant Location_Vectors.Vector := Near (This, L);
         begin
            if This.Data (L.Y, L.X) = Free then
               return L;
            else
               for I in N.First_Index .. N.Last_Index loop
                  if not Visited.contains (N.Element (I)) then
                     Pending.Append (N.Element (I));
                  end if;
               end loop;
               Visited.Include (Pending.First_Element);
               Pending.Delete_First;
            end if;
         end;
      end loop;

      raise Constraint_Error with "No free cells in given neighborhood?";
   end Get_Closest_Free_To;

   ----------------
   -- Vicinity_8 --
   ----------------

   function Vicinity_8 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector
   is
      X : constant Abscissa := Bit_Location (L).X;
      Y : constant Ordinate := Bit_Location (L).Y;
      R : Location_Vectors.Vector;
      This : Object'Class renames Object'Class (M);
      Xd   : constant array (1 .. 8) of Abscissa :=
               (1, -1,  0,  0, 1,  1, -1, -1);
      Yd   : constant array (1 .. 8) of Ordinate :=
               (0,  0,  1, -1, 1, -1,  1, -1);
   begin
      for I in Xd'Range loop
         if This.Within_Bounds (X + Xd (I), Y + Yd (I)) and then
           This.Data (Y + yd (I), X + Xd (I)) = Free
         then
            R.Append (Bit_Location'(M => This.Bself,
                                    X => X + Xd (I),
                                    Y => Y + Yd (I)));
         end if;
      end loop;
      return R;
   end Vicinity_8;

   ----------------
   -- Vicinity_6 --
   ----------------

   function Vicinity_6 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector
   is
      X : constant Abscissa := Bit_Location (L).X;
      Y : constant Ordinate := Bit_Location (L).Y;
      R : Location_Vectors.Vector;
      This : Object'Class renames Object'Class (M);
      Xd   : constant array (1 .. 8) of Abscissa :=
               (1, -1,  0,  0, 1,  1, -1, -1);
      Yd   : constant array (1 .. 8) of Ordinate :=
               (0,  0,  1, -1, 1, -1,  1, -1);
   begin
      for I in Xd'Range loop
         if This.Within_Bounds (X + Xd (I), Y + Yd (I)) and then
           This.Data (Y + yd (I), X + Xd (I)) = Free
         then
            if
              Yd (I) = 0 or else Xd (I) = 0 or else
              (This.Data (Y + yd (I), X +      0) = Free and then
               This.Data (Y +      0, X + Xd (I)) = Free)
            then
               R.Append (Bit_Location'(M => This.Bself,
                                       X => X + Xd (I),
                                       Y => Y + Yd (I)));
               pragma Assert (L.Is_Neighbor (R.Last_Element));
            end if;
         end if;
      end loop;
      return R;
   end Vicinity_6;

   ----------------
   -- Vicinity_4 --
   ----------------

   function Vicinity_4 (M : Map.Object'Class;
                        L : Location'Class) return Location_Vectors.Vector
   is
      X      : constant Abscissa := Bit_Location (L).X;
      Y      : constant Ordinate := Bit_Location (L).Y;
      Xdelta : constant array (1 .. 4) of Abscissa := (-1, 1,  0, 0);
      Ydelta : constant array (1 .. 4) of Ordinate := ( 0, 0, -1, 1);
      R : Location_Vectors.Vector;
      This : Object'Class renames Object'Class (M);
   begin
      for I in Xdelta'Range loop
         if This.Within_Bounds (X + Xdelta (I), Y + Ydelta (I)) and then
           This.Data (Y + Ydelta (I), X + Xdelta (I)) = Free
         then
            R.Append (Bit_Location'(M => This.Bself,
                                    X => X + Xdelta (I),
                                    Y => Y + Ydelta (I)));
         end if;
      end loop;

      if R.Is_Empty then
         Log ("S.Map.Bitmap.Vicinity_4: No neighbors?", Error);
         raise Constraint_Error with "No neighbors";
      end if;
      return R;
   end Vicinity_4;

   --------------
   -- Any_At_4 --
   --------------

   function Any_At_4 (M : Map.Object'Class;
                      L : Location'Class) return Location_Vectors.Vector
   is
      X      : constant Abscissa := Bit_Location (L).X;
      Y      : constant Ordinate := Bit_Location (L).Y;
      Xdelta : constant array (1 .. 4) of Abscissa := (-1, 1,  0, 0);
      Ydelta : constant array (1 .. 4) of Ordinate := ( 0, 0, -1, 1);
      R      : Location_Vectors.Vector;
      This   : Object'Class renames Object'Class (M);
   begin
      for I in Xdelta'Range loop
         if This.Within_Bounds (X + Xdelta (I), Ydelta (I)) then
            R.Append (Bit_Location'(M => This.Bself,
                                    X => X + Xdelta (I),
                                    Y => Y + Ydelta (I)));
         end if;
      end loop;

      if R.Is_Empty then
         Log ("S.Map.Bitmap.Vicinity_4: No neighbors?", Error);
         raise Constraint_Error with "No neighbors";
      end if;
      return R;
   end Any_At_4;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (M : Map.Object'Class;
                      O : Observation'Class) return Costs
   is
      pragma Unreferenced (M);
      C : constant array (Terrains) of Costs :=
            (Free => 1.0, Obstacle => Infinite);
   begin
      return C (Bit_Observation (O).Bit);
   end Get_Cost;
   --  1 for Free, Inf for Obstacle

   -------------------
   -- Compute_Costs --
   -------------------

   procedure Compute_Costs
     (Map      : in out Object;
      Get_Near : access function (M : Sancta.Map.Object'Class;
                                  L : Location'Class)
                                  return Location_Vectors.Vector;
      Get_Cost : access function (M : Sancta.Map.Object'Class;
                                  O : Observation'Class) return Costs)
   is
      This : Object renames Map;

      function Create is new Create_Wavefront (Get_Near.all, Get_Cost.all);

   begin
      Free (This.Cost);
      This.Cost := new Cost_Matrix (This.Data'Range, This.Data'Range (2),
                                    This.Data'Range, This.Data'Range (2));

      --  Loop over destinations
      for R in This.Data'Range loop
         for C in This.Data'Range (2) loop
            if This.Data (R, C) = Free then
               declare
                  Wave : constant Loc_Cost_Maps.Map :=
                           Create (Bit_Location'(This.Bself, C, R), This);
               begin
                  for R2 in This.Data'Range loop
                     for C2 in This.Data'Range (2) loop
                        if This.Data (R2, C2) = Free then
                           This.Cost (R2, C2, R, C) :=
                             Wave.Element (Bit_Location'(This.Bself, C2, R2));
                           This.Cost (R, C, R2, C2) :=
                             Wave.Element (Bit_Location'(This.Bself, C2, R2));
                        end if;
                     end loop;
                  end loop;
               end;
            end if;
         end loop;
      end loop;
   end Compute_Costs;

   -------------------
   -- Compute_Costs --
   -------------------

   procedure Compute_Costs (This : in out Object) is
      Total : constant Natural :=
                This.Data'Length *
                This.Data'Length (2) *
                This.Data'Length *
                This.Data'Length (2);
      Done  :          Natural := 0;
      use Types.Operations;
   begin
      Free (This.Cost);
      This.Cost := new Cost_Matrix (This.Data'Range, This.Data'Range (2),
                                    This.Data'Range, This.Data'Range (2));

      --  Loop over destinations
      for R in This.Data'Range loop
         for C in This.Data'Range (2) loop
            for R2 in This.Data'Range loop
               for C2 in This.Data'Range (2) loop
                  if
                    This.Data (R, C) = Free and then This.Data (R2, C2) = Free
                  then
                     This.Cost (R2, C2, R, C) :=
                       This.Best_Path (This.Loc (C, R),
                                       This.Loc (C2, R2)).Cost;
                     --  Bad approximation:
--                         Costs
--                           (Distance
--                                (This.Nearest_Pose (This.Loc (C, R)),
--                                 This.Nearest_Pose (This.Loc (C2, R2))));
                  else
                     This.Cost (R, C, R2, C2) := Infinite;
                     This.Cost (R2, C2, R, C) := Infinite;
                  end if;
                  Done := Done + 1;
               end loop;
            end loop;
            Log ("Done:" & Natural'Image (Done * 100 / Total) & "%",
                 Debug, Log_Section);
         end loop;
      end loop;
   end Compute_Costs;

   ----------------------
   -- Get_Cost_Between --
   ----------------------

   function Get_Cost_Between
     (This : Object;
      X1    : Abscissa;
      Y1    : Ordinate;
      X2    : Abscissa;
      Y2    : Ordinate) return Costs is
   begin
      if This.Cost /= null then
         return This.Cost (Y1, X1, Y2, X2);
      else
         return This.Best_Path (This.Loc (X1, Y1), This.Loc (X2, Y2)).Cost;
      end if;
   end Get_Cost_Between;

   --------------------
   -- Stage_Location --
   --------------------

   function Stage_Location (Map  : Sancta.Map.Object'Class;
                            Row  : Rows;
                            Col  : Columns)
                            return Sancta.Map.Location'Class
   is
      M : Object renames Object (Map);
   begin
      return Bit_Location'
        (M => M.Bself,
         X => Col,
         Y => M.Data'Last - Row);
   end Stage_Location;

   -----------------------
   -- Stage_Observation --
   -----------------------

   function Stage_Observation (Sample : Agpl.Types.Rgb_Triplet)
                               return   Map.Observation'Class
   is
      use type Agpl.Types.Unsigned_8;
      Equiv : constant array (Boolean) of Terrains :=
                (False => Free, True => Obstacle);
   begin
      return Bit_Observation'
        (Bit => Equiv
           (Sample.R < 128 or else Sample.G < 128 or else Sample.B < 128));
   end Stage_Observation;


   -----------
   -- Print --
   -----------

   procedure Print (This : Object; Route : Location_Lists.List) is
      use Location_Lists;

      M : Object (This.Vicinity);

      procedure Mark (I : Cursor) is
      begin
         M.Set_At (Bit_Location (Element (I)),
                   Bit_Observation'(Bit => Obstacle));
      end Mark;
   begin
      M.Set_Size (This.Data'First (2), This.Data'First,
                  This.Data'Last (2), This.Data'Last);
      for I in M.Data'Range loop
         for J in M.Data'Range (2) loop
            M.Data (I, J) := Free;
         end loop;
      end loop;

      Route.Iterate (Mark'Access);

      M.Print;
   end Print;

   -------------
   -- To_Grid --
   -------------

   function To_Grid (This  : Object;
                     Pose  : Types.Pose) return Map.Bitmap.Bit_Location'Class is
   begin
      return Bit_Location'
        (M => This.Bself,
         X => Abscissa (Real'Floor (Pose.X / Real (This.Cell_Size))),
         Y => Ordinate (Real'Floor (Pose.Y / Real (This.Cell_Size))));
   end To_Grid;

   -------------
   -- To_Pose --
   -------------

   function To_Pose (Ratio : Float;
                     Loc   : Map.Bitmap.Bit_Location) return Types.Pose is
   begin
      return (X => Real (Loc.X) * Real (Ratio) + Real (Ratio / 2.0),
              Y => Real (Loc.Y) * Real (Ratio) + Real (Ratio / 2.0),
              A => 0.0);
   end To_Pose;

   -------------
   -- To_Pose --
   -------------

   function To_Pose (This  : Object;
                     Loc   : Map.Bitmap.Bit_Location'Class) return Types.Pose is
   begin
      return To_Pose (This.Cell_Size, Loc);
   end To_Pose;

   -------------------
   -- Within_Bounds --
   -------------------

   function Within_Bounds (This : Object;
                           X    : Abscissa;
                           Y    : Ordinate) return Boolean
   is
   begin
      return X in This.Data'Range (2) and then Y in This.Data'Range (1);
   end Within_Bounds;

   -------------------
   -- Set_Cell_Size --
   -------------------

   procedure Set_Cell_Size (This : in out Object; Cell_Size : Float) is
   begin
      This.Cell_Size := Cell_Size;
   end Set_Cell_Size;

   -------------------
   -- Get_Cell_Size --
   -------------------

   function Get_Cell_Size (This : Object) return Float is
   begin
      return This.Cell_Size;
   end Get_Cell_Size;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance (This : Object;
                          Ini,
                          Fin  : Location'Class) return Types.Real
   is
      use Sancta.Types.Operations;
   begin
      return Distance (To_Pose (This.Cell_Size, Bit_Location (Ini)),
                       To_Pose (This.Cell_Size, Bit_Location (Fin)));
   end Get_Distance;

   --------------------
   -- Get_Distancing --
   --------------------
   --  At each step, we try to advance Ini, move back Fin, and Both.
   --  The movement that causes less distance is chosen.
   function Get_Distancing (This : Object;
                            Path : Location_Vectors.Vector) return Types.Real
   is
      use Location_Vectors;
      use Sancta.Types.Operations;
      Max : Real := This.Get_Distance (Path.First_Element, Path.Last_Element);

      Ini : Positive := Path.First_Index;
      Fin : Positive := Path.Last_Index;

      Ini_Delta : constant array (1 .. 3) of Integer := ( 1, 1,  0);
      Fin_Delta : constant array (1 .. 3) of Integer := (-1, 0, -1);
   begin
      if Natural (Path.Length) <= 3 then
         return Max;
      end if;

      while Ini < Fin loop
         declare
            Best_Move : Positive;
            Move_Min  : Real := Real'Last;
            Move_Dist : Real;
         begin
            for I in Ini_Delta'Range loop
               Move_Dist :=
                 This.Get_Distance (Path.Element (Ini + Ini_Delta (I)),
                                    Path.Element (Fin + Fin_Delta (I)));
               if Move_Dist < Move_Min then
                  Move_Min  := Move_Dist;
                  Best_Move := I;
               end if;
            end loop;
            Max := Real'Max (Max, Move_Min);
            Ini := Ini + Ini_Delta (Best_Move);
            Fin := Fin + Fin_Delta (Best_Move);
         end;
      end loop;
      return Max;
   exception
      when E : others =>
         Log ("Get_Distancing: " & Report (E), Warning, Log_Section);
         return Max;
   end Get_Distancing;

   ----------------------
   -- Get_Min_Distance --
   ----------------------

   function Get_Min_Distance (This : Object;
                              Pose : Types.Pose;
                              Path : Map.Path) return Types.Real
   is
      Min : Types.Real := Types.Real'Last;
      use Map.Paths;
      procedure Check (I : Cursor) is
      begin
         Min := Types.Real'Min
           (Min, Operations.Distance
              (Pose,
               To_Pose (This.Cell_Size, Bit_Location (Element (I)))));
      end Check;
   begin
      Path.Iterate (Check'Access);
      return Min;
   end Get_Min_Distance;

   ------------------
   -- Get_Min_Cost --
   ------------------

   function Get_Min_Cost (This : Object;
                          Loc  : Bit_Location'Class;
                          Path : Map.Path) return Costs
   is
      Min : Costs := Infinite;
      use Map.Paths;
      procedure Check (I : Cursor) is
         Ploc : Bit_Location renames Bit_Location (Element (I));
      begin
         Min := Costs'Min (Min, This.Get_Cost_Between
                           (Loc.X, Loc.Y, Ploc.X, Ploc.Y));
      end Check;
   begin
      Path.Iterate (Check'Access);
      return Min;
   end Get_Min_Cost;

   -----------------------
   -- Get_Min_Cost_Cell --
   -----------------------

   function Get_Min_Cost_Cell (This : Object;
                               Loc  : Bit_Location'Class;
                               Path : Map.Path) return Bit_Location'Class
   is
      Min  : Costs         := Infinite;
      Best : Bit_Location;
      use Map.Paths;
      procedure Check (I : Cursor) is
         Ploc : Bit_Location renames Bit_Location (Element (I));
         Cost : constant Costs := This.Get_Cost_Between
           (Loc.X, Loc.Y, Ploc.X, Ploc.Y);
      begin
         if Cost < Min then
            Best := Ploc;
            Min  := Cost;
         end if;
      end Check;
   begin
      Path.Iterate (Check'Access);
      return Best;
   end Get_Min_Cost_Cell;

   ---------------
   -- Is_U_Turn --
   ---------------

   function Is_U_Turn (Path : Map.Path) return Boolean
   is
      Base_X : constant Abscissa := Bit_Location (Path.First_Element).X;
      Base_Y : constant Ordinate := Bit_Location (Path.First_Element).Y;

      Dx : constant Abscissa :=
             Bit_Location (Path.Last_Element).X -
             Bit_Location (Path.First_Element).X;
      Dy : constant Ordinate :=
             Bit_Location (Path.Last_Element).Y -
             Bit_Location (Path.First_Element).Y;
   begin
      if Natural (Path.Length) <= 2 then
         return False;
      else
         declare
            use Paths;
            I : Cursor := Next (Path.First);
         begin
            while Has_Element (I) loop
               declare
                  Loc : Bit_Location renames Bit_Location (Element (I));
               begin
                  if
                    Dx * (Loc.X - Base_X) < 0 or else
                    Dy * (Loc.Y - Base_Y) < 0
                  then
                     Log ("Detected U-TURN", Never);
                     return True;
                  end if;
               end;
               Next (I);
            end loop;
         end;
         return False;
      end if;
   end Is_U_Turn;

   --------------------
   -- Copy_But_Costs --
   --------------------

   procedure Copy_But_Costs (This :     Object;
                             Copy : out Object)
   is
   begin
      --  Avoid Adjust
      Copy.Data := new Matrix'(This.Data.all);
      Copy.Cost := null;
      Copy.Cell_Size := This.Cell_Size;
   end Copy_But_Costs;

   ----------
   -- Read --
   ----------

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object)
   is
   begin
      This.Finalize;
      This.Data      := new Matrix'(Matrix'Input (Stream));
      This.Cost      := new Cost_Matrix (This.Data'Range, This.Data'Range (2),
                                         This.Data'Range, This.Data'Range (2));
      Cost_Matrix'Read (Stream, This.Cost.all);
      This.Cell_Size := Float'Input (Stream);
      Log ("loaded matrix: " & To_String (This.Cell_Size), Always, Log_Section);
      Log ("loaded matrix: " & This.Hash, Always, Log_Section);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object)
   is
   begin
      Matrix'Output      (Stream, This.Data.all);
      Cost_Matrix'Write  (Stream, This.Cost.all);
      Float'Output       (Stream, This.Cell_Size);
      Log ("saved matrix: " & To_String (This.Cell_Size), Always, Log_Section);
   end Write;

   ----------
   -- Hash --
   ----------

   function Hash (This : Object) return String is
      use Gnat.Md5;
      Fgprint : Context;
   begin
      Update (Fgprint, External_Tag (Object'Tag));
      Update (Fgprint, This.Cell_Size'Img);
      for R in This.Data'Range loop
         for C in This.Data'Range (2) loop
            Update (Fgprint, This.Data (R, C)'Img);
         end loop;
      end loop;

      return Digest (Fgprint);
   end Hash;

   --------------
   -- Contains --
   --------------

   function Contains (Path : Map.Path; Loc : Bit_Location) return Boolean is
      use Paths;
      I : Cursor := Path.First;
   begin
      while Has_Element (I) loop
         if Bit_Location (Element (I)) = Loc then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

   ---------------
   -- Direction --
   ---------------

   function Direction (L1, L2 : Bit_Location) return Directions is
      Dir : constant array (Abscissa'(-1) .. 1,
                            Ordinate'(-1) .. 1) of Directions :=
                                            (-1 => (-1 => So,
                                                    0  => O,
                                                    1  => No),
                                             0  => (-1 => S,
                                                    0  => S,
                                                    1  => N),
                                             1  => (-1 => Se,
                                                    0  => E,
                                                    1  => Ne));
   begin
      if L1 = L2 then
         raise Constraint_Error with "No direction for same location";
      else
         return Dir (L2.X - L1.X, L2.Y - L1.Y);
      end if;
   end Direction;

   -----------
   -- Image --
   -----------

   function Image (Path : Map.Path) return String is
      use Map.Paths;
      S : String (1 .. Natural (Path.Length) - 1);
      J : Positive := S'First;
      procedure Print (I : Cursor) is
      begin
         if I /= Path.First then
            S (J) :=
              Directions_Img
                (Direction
                     (Bit_Location (Element (Previous (I))),
                      Bit_Location (Element (I))));
            J := J + 1;
         end if;
      end Print;
   begin
      Path.Iterate (Print'Access);
      return S;
   end Image;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (This : Object;
                       Path : Map.Path) return Sancta.Types.Pose_Vector.Vector is
      V : Sancta.Types.Pose_Vector.Vector;

      use Sancta.Map.Location_Lists;

      procedure Convert (I : Cursor) is
      begin
         V.Append (This.To_Pose (Map.Bitmap.Bit_Location (Element (I))));
      end Convert;
   begin
      Path.Iterate (Convert'Access);
      return V;
   end To_Vector;

   -------------------
   -- Num_Neighbors --
   -------------------

   function Num_Neighbors (L : Bit_Location)
                           return Natural
   is
   begin
      case L.M.Vicinity is
         when Vicinity_4 =>
            return Natural (Vicinity_4 (L.M.all, L).Length);
         when Vicinity_6 =>
            return Natural (Vicinity_6 (L.M.all, L).Length);
         when Vicinity_8 =>
            return Natural (Vicinity_8 (L.M.all, L).Length);
      end case;
   end Num_Neighbors;

   --------------
   -- Neighbor --
   --------------

   function Neighbor (L : Bit_Location; I : Positive)
                      return Bit_Location
   is
      pragma Hugely_Inneficient;
      --  We are computing all neighbors in each call!!!!!!!
   begin
      case L.M.Vicinity is
         when Vicinity_4 =>
            return Bit_Location (Vicinity_4 (L.M.all, L).Element (I));
         when Vicinity_6 =>
            return Bit_Location (Vicinity_6 (L.M.all, L).Element (I));
         when Vicinity_8 =>
            return Bit_Location (Vicinity_8 (L.M.all, L).Element (I));
      end case;
   end Neighbor;

   ---------------
   -- Real_Cost --
   ---------------

   function Real_Cost (Ini, Fin : Bit_Location) return Costs is
      use Types.Operations;
   begin
      --  Not sure if it's quicker to determine if it's neighbor + dist comput.
      --  Or a cost lookup.
      --  However, since the loading of cached costs is failing me just now,
      --    I'm gonna keep this this way...

      if Ini.Is_Neighbor (Fin) then
--           Log ("Returning adjacent cost", Always);
         return Costs (Distance (Ini.M.To_Pose (Ini), Ini.M.To_Pose (Fin)));
      elsif Ini.M.Cost /= null then
--           Log ("Returning cached cost", Always);
         return Ini.M.Get_Cost_Between (Ini.X, Ini.Y, Fin.X, Fin.Y);
      else
--           Log ("Returning computed cost", Always);
         return Ini.M.Get_Cost_Between (Ini.X, Ini.Y, Fin.X, Fin.Y);
      end if;
   end Real_Cost;

   -------------------
   -- Estimate_Cost --
   -------------------

   function Estimate_Cost (Ini, Fin : Bit_Location) return Costs is
      use Sancta.Types.Operations;
   begin
      return Costs (Distance (Ini.M.To_Pose (Ini), Ini.M.To_Pose (Fin)));
   end Estimate_Cost;

   -----------------
   -- Is_Neighbor --
   -----------------
   Near_Table : constant array (Vicinities,
                       Abscissa'(0) .. 1,
                       Ordinate'(0) .. 1) of Boolean :=
         (Vicinity_4 => (0 => (0 => False,
                               1 => True),
                         1 => (0 => True,
                               1 => False)),
          Vicinity_6 => (0 => (0 => False,
                               1 => True),
                         1 => (0 => True,
                               1 => False)),
          Vicinity_8 => (0 => (0 => False,
                               1 => True),
                         1 => (0 => True,
                               1 => True)));
   function Is_Neighbor (L, R : Bit_Location) return Boolean is
      Dx : constant Abscissa := abs (L.X - R.X);
      Dy : constant Ordinate := abs (L.Y - R.Y);
   begin
      if Bit_Observation (R.M.Get_At (R)).Bit /= Free then
         return False;
      end if;
      case L.M.Vicinity is
         when Vicinity_4 | Vicinity_8 =>
            if Dx > 1 or else Dy > 1 then
               return False;
            else
               return Near_Table (L.M.Vicinity, Dx, Dy);
            end if;
         when Vicinity_6 =>
            if Dx > 1 or else Dy > 1 then
               return False;
            elsif Near_Table (L.M.Vicinity, Dx, Dy) then
               return True;
            else
               declare
                  Dx : constant Abscissa := (R.X - L.X);
                  Dy : constant Ordinate := (R.Y - L.Y);
               begin
                  return
                    (Dx /= 0 or else Dy /= 0) and then
                    (L.M.Data (L.Y + Dy, L.X +     0) = Free and then
                     L.M.Data (L.Y +  0, L.X +    Dx) = Free);
               end;
            end if;
      end case;
   end Is_Neighbor;

   --------------
   -- Position --
   --------------

   function Position (Here, There : Bit_Location)
                      return        Relative_Position'Class
   is
   begin
      return Bit_Rel_Pos'(Dir => Direction (Here, There));
   end Position;

   --------------
   -- Location --
   --------------

   function Loc (This : Object;
                 X    : Abscissa;
                 Y    : Ordinate) return Bit_Location'Class is
   begin
      return Bit_Location'Class (Bit_Location'(This.Bself, X, Y));
   end Loc;

   ----------------------
   -- Nearest_Location --
   ----------------------

   function Nearest_Location (This : Object;
                              Pose : Types.Pose) return Location'Class
   is
   begin
      return Location'Class (This.To_Grid (Pose));
   end Nearest_Location;

   ------------------
   -- Nearest_Pose --
   ------------------

   function Nearest_Pose (This : Object;
                          Loc  : Location'Class) return Types.Pose
   is
   begin
      return This.To_Pose (Bit_Location (Loc));
   end Nearest_Pose;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Bit_Rel_Pos) return Boolean is
   begin
      return L.Dir < R.Dir;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (This : Bit_Rel_Pos) return String is
   begin
      return This.Dir'Img;
   end Image;

   --------------------
   -- Is_Traversable --
   --------------------

   function Is_Traversable (This : Bit_Observation) return Boolean is
   begin
      return This.Bit = Free;
   end Is_Traversable;

   procedure Draw
     (This    :        Object;
      D       : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Constants;
   begin
      D.Set_Color (White, Alpha_Opaque);
      D.Fill_Rectangle (Float (This.Get_X_Min),
                        Float (This.Get_Y_Min),
                        Float (This.Get_X_Max + 1),
                        Float (This.Get_Y_Max + 1));

      D.Set_Color (Black, Alpha_Opaque);
      D.Draw_Rectangle (Float (This.Get_X_Min),
                        Float (This.Get_Y_Min),
                        Float (This.Get_X_Max + 1),
                        Float (This.Get_Y_Max + 1));

      for R in This.Data'Range loop
         for C in This.Data'Range (2) loop
            if This.Data (R, C) = Obstacle then
               D.Fill_Rectangle (Float (C),
                                 Float (R),
                                 Float (C + 1),
                                 Float (R + 1));
            end if;
         end loop;
      end loop;
   end Draw;

end Sancta.Map.Bitmap;
