--  Binding to the C MbICP library

--  THIS LIBRARY IS NOT MULTITHREADED NOR REENTRANT!!

with Interfaces.C; use Interfaces;

package Mbicp is

   pragma Elaborate_Body;

   type Mbicp_Float is new C.C_Float;

   --  Each point detected by the laser, in polar coordinates:
   type Laser_Reading is record
      Distance,
      Bearing  : Mbicp_Float;
   end record;
   pragma Convention (C, Laser_Reading);

   type Laser_Scan is array (Positive range <>) of Laser_Reading;
   pragma Convention (C, Laser_Scan);

   --  A pose
   type Pose is record
      X,
      Y,
      A : Mbicp_Float;
   end record;
   pragma Convention (C, Pose);

   procedure Set_Out_Of_Range (Distance : in Mbicp_Float);
   --  Any reading farther than this is discarded.

   procedure Init (Bw                : in Float    := 1.57 / 3.0;
                   --  Trimming maximum angular distance between two scans
                   --  to reject match. Speed-up.

                   Br                : in Float    := 0.09;
                   --  Trimming maximum linear distance to reject match.

                   L                 : in Float    := 3.0;
                   --  L -> Inf : favors translation errors
                   --  L -> 0   : favors rotation errors

                   Laser_Step        : in Positive := 1;
                   --  1 -> Use all readings
                   --  2 -> Use one of each two readings
                   --  ...

                   Max_Dist_Inter    : in Float    := 0.5;
                   --  Resolution of "holes": no hole is expected between
                   --  two laser readings larger than MDI ** 2.

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
                  );
   --  Set parameters for the algorithm. Defaults apply if you never call it.

   type Outcomes is (Ok_Converged,     Ok_Exhausted,
                     Fail_Association, Fail_Minimization);

   subtype Outcomes_Ok   is Outcomes range Ok_Converged .. Ok_Exhausted;
   subtype Outcomes_Fail is Outcomes range Fail_Association .. Fail_Minimization;

   procedure Match (Laser1,
                    Laser2  : in     Laser_Scan;
                    Odom    : in     Pose;
                    Result  :    out Pose;
                    Outcome :    out Outcomes);
   --  Given two laser scans and an estimation of odometry,
   --  compute the true Result odometry movement.
   --  Result is valid only if Outcome_Ok

   function Get_Num_Filtered_Associations return Natural;
   function Get_Num_Associations (Max_Dist : in MbIcp_Float := MbIcp_Float'Last)
                                  return        Natural;
   --  These can be used to get some idea of the "quality" of the found match.

   function Get_Mean_Error return Mbicp_Float;
   --  Get the mean error of the filtered associations

   pragma Inline (Get_Num_Filtered_Associations,
                  Get_Num_Associations);

end Mbicp;
