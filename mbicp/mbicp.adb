package body Mbicp is

   use type C.C_Float;
   use type C.Int;

   Max_Laser_Range : C.C_Float;
   pragma Import (C, Max_Laser_Range, "MAXLASERRANGE");

   function C_Fixed_Num_Readings return C.Int;
   pragma Import (C, C_Fixed_Num_Readings, "C_Fixed_Num_Readings");

   Fixed_Num_Readings : constant Natural := Natural (C_Fixed_Num_Readings);

   ----------------------
   -- Set_Out_Of_Range --
   ----------------------

   procedure Set_Out_Of_Range (Distance : in Mbicp_Float) is
   begin
      Max_Laser_Range := C.C_Float (Distance);
   end Set_Out_Of_Range;

   ----------
   -- Init --
   ----------

   procedure Init (Bw                : in Float    := 1.57 / 3.0;
                   --  Trimming maximum angular distance between two scans
                   --  to reject match. Speed-up.

                   Br                : in Float    := 0.09;
                   --  Trimming maximum linear distance to reject match.

                   L                 : in Float    := 3.0;
                   --  L -> Inf : standard ICP
                   --  L -> 0   : metric (favors rotation?)

                   Laser_Step        : in Positive := 1;
                   --  1 -> Use all readings
                   --  2 -> Use one of each two readings
                   --  ...

                   Max_Dist_Inter    : in Float    := 0.5;
                   --  Distance for interpolation of what???

                   Filter            : in Float    := 0.85;
                   --  Fraction of correspondences used during minimization.

                   Projection_Filter : in Boolean  := True;
                   --  Discard non-visible points from the two poses.
                   --  Lu & Millios 97
                   --  Works well below 45º

                   Assoc_Error       : in Float    := 0.1;
                   --  Ratio saying the minimum associations that should be
                   --  made. Error otherwise.

                   Max_Iter          : in Positive := 50;
                   --  Iterations allowed

                   Error_Ratio       : in Float    := 0.0001;
                   --  Ratio between two iterations, so it goes to 0.0 when
                   --  convergence is good.

                   Err_X_Out         : in Float    := 0.0001;
                   Err_Y_Out         : in Float    := 0.0001;
                   Err_A_Out         : in Float    := 0.0001;
                   --  Error in each component. The three must be satisfied
                   --  to consider convergence reached. Goes to 0.0.

                   Iter_Smooth_Conv  : in Positive := 2
                   --  Number of consecutive iterations that must satisfy the
                   --  convergence criteria. It gives stability (?)
                  )
   is
      ------------
      -- Init_C --
      ------------

      procedure Init_C
        (Bw                : in C.C_Float;
         Br                : in C.C_Float;
         L                 : in C.C_Float;
         Laser_Step        : in C.Int;
         Max_Dist_Inter    : in C.C_Float;
         Filter            : in C.C_Float;
         Projection_Filter : in C.Int;
         Assoc_Error       : in C.C_Float;
         Max_Iter          : in C.Int;
         Error_Ratio       : in C.C_Float;
         Err_X_Out         : in C.C_Float;
         Err_Y_Out         : in C.C_Float;
         Err_A_Out         : in C.C_Float;
         Iter_Smooth_Conv  : in C.Int);
      pragma Import (C, Init_C, "Init_MbICP_ScanMatching");

      To_Int : constant array (Boolean) of C.Int := (False => 0, True => 1);
   begin
      Init_C (C.C_Float (Bw),
              C.C_Float (Br),
              C.C_Float (L),
              C.Int (Laser_Step),
              C.C_Float (Max_Dist_Inter),
              C.C_Float (Filter),
              To_Int (Projection_Filter),
              C.C_Float (Assoc_Error),
              C.Int (Max_Iter),
              C.C_Float (Error_Ratio),
              C.C_Float (Err_X_Out),
              C.C_Float (Err_Y_Out),
              C.C_Float (Err_A_Out),
              C.Int (Iter_Smooth_Conv));
   end Init;

   -----------
   -- Match --
   -----------

   procedure Match
     (Laser1,
      Laser2  : in     Laser_Scan;
      Odom    : in     Pose;
      Result  :    out Pose;
      Outcome :    out Outcomes)
   is
      L1, L2 : Laser_Scan (1 .. Fixed_Num_Readings);
   begin
      --  Set to out of range unused positions:
      L1 (1 .. Laser1'Length) := Laser1;
      L2 (1 .. Laser2'Length) := Laser2;

      for I in Laser1'Length + 1 .. L1'Last loop
         L1 (I).Distance := Mbicp_Float (Max_Laser_Range) + 1.0;
      end loop;

      for I in Laser2'Length + 1 .. L2'Last loop
         L2 (I).Distance := Mbicp_Float (Max_Laser_Range) + 1.0;
      end loop;

      declare
         procedure C_Match (Code   :    out C.Int;
                            L1, L2 : in out Laser_Scan;
                            Odom   : access Pose;
                            Result :    out Pose);
         pragma Import (C, C_Match);
         pragma Import_Valued_Procedure (Internal  => C_Match,
                                         External  => "MbICPmatcher");

         Code : C.Int;
      begin
         C_Match (Code, L1, L2, Odom'Unrestricted_Access, Result);

         case Code is
            when  1     => Outcome := Ok_Converged;
            when  2     => Outcome := Ok_Exhausted;
            when -1     => Outcome := Fail_Association;
            when -2     => Outcome := Fail_Minimization;
            when others => raise Constraint_Error;
         end case;
      end;
   end Match;

   -----------------------------------
   -- Get_Num_Filtered_Associations --
   -----------------------------------

   function Get_Num_Filtered_Associations return Natural is
      Num : C.Int;
      pragma Import (C, Num, "cntAssociationsTemp");
   begin
      return Natural (Num);
   end Get_Num_Filtered_Associations;

   --------------------------
   -- Get_Num_Associations --
   --------------------------

   function Get_Num_Associations (Max_Dist : in MbIcp_Float := MbIcp_Float'Last)
                                  return        Natural
   is
      Num : C.Int;
      pragma Import (C, Num, "cntAssociationsT");

      function C_Num_Associations (Max_Dist : in C.C_Float) return C.Int;
      pragma Import (C, C_Num_Associations, "C_Num_Associations");
   begin
      if Max_Dist = MbIcp_Float'Last then
         return Natural (Num);
      else
         return Natural (C_Num_Associations (C.C_Float (Max_Dist)));
      end if;
   end Get_Num_Associations;

   --------------------
   -- Get_Mean_Error --
   --------------------

   function Get_Mean_Error return Mbicp_Float is
      function C_Mean_Error return C.C_Float;
      pragma Import (C, C_Mean_Error, "C_Mean_Error");
   begin
      return Mbicp_Float (C_Mean_Error);
   end Get_Mean_Error;

begin
   Init;
end Mbicp;
