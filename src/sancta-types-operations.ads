with Agpl.Constants;
with Sancta.Gap;
with Agpl.Transf2D;

package Sancta.Types.Operations is

   pragma Preelaborate;

   Log_Section : constant String := "sancta.types.operations";
   Log_Detail  : constant String := "sancta.types.operations.detail";

   package Real_Transf is new Agpl.Transf2D (Types.Real);

   function Best_Observation_Pose (A, B : in Pose;
                                   FOV  : in Float) return Pose;
   --  Given two ordered left-to-right points, compute the best observation pose
   --  given the FOV.

   function Distance (A, B : in Pose) return Real;

   function Modulus (P : in Pose) return Real;

   function Time (A, B        : in Pose;
                  Lin_Speed   : in Real;
                  Ang_Speed   : in Real;
                  Use_A_Angle : in Boolean := True;
                  Use_B_Angle : in Boolean := True) return Real;

   function Offset (P, Q : Pose) return Angle;
   --  Given oriented pose P, Angle from P to Q minus P.A.
   --  In other words, the amount we should turn at P to be looking at Q.

   function To_Pose (P : in Real_Transf.Pose) return Types.Pose;
   function "+"     (P : in Real_Transf.Pose) return Types.Pose renames To_Pose;
   pragma Inline (To_Pose);

   function To_Pose (P : in Types.Pose) return Real_Transf.Pose;
   function "+"     (P : in Types.Pose) return Real_Transf.Pose renames To_Pose;

   type Merging_Policies is (No, Always);
   --  No: No merging is done.
   --  Always: Two consecutive gaps are merged into one. Can cause problems in
   --  totally open spaces with 180 FOV.

   function Extract_Gaps (This      : Range_Scan;
                          Epsilon   : Float := 0.35;
                          Min_Width : Float := 1.0;
                          Horizon   : Float := Float'Last;
                          Merge     : Merging_Policies := Always)
                          return Sancta.Gap.Object_Array;
   --  Extract the gaps in a range reading.
   --  The gaps are returned in robot coordinates.
   --  Epsilon is the distance around the robot used for gap validity test.
   --  0.35 is "optimal" for 8m laser. This should be computed properly somewhere.
   --  Min_Width is the minimum distance between two laser readings for them to
   --  form an Occlusion gap.
   --  Horizon: range reading farther than this are cut down. (considered out of range).
   --  The gap vertices are in counter-clockwise ordering.

   function Segment_Ratio (X, Y  : in Pose;
                           Ratio : in Float) return Pose;
   --  Given the XY segment, provides the point along the line in XY sense which
   --  corresponds to the given ratio. For example, 0.5 would give the middle point.
   --  The angle is X.A + Ratio * (Y.A - X.A) as well.

   No_Angle : exception;

   function Angle_From_Scan (Scan         : in Range_Scan;
                             Out_Of_Range : in Real) return Angle;
   --  Given a raw range scan, determine the dominating angle in the lines
   --  seen in the environment.
   --  May raise No_Angle on errors or insufficient data.
   --  The angle is returned in robocentric frame (X is forward)

   No_Correction : exception;

   function Corrected_Pose (Robot_Pose      : in Pose;
                            Angle_From_Scan : in Angle;
                            Guides          : in Angle :=
                              Angle (Agpl.Constants.Pi_2);
                            Tolerance       : in Angle := 0.17) return Pose;
   --  Given a robot pose, the seen dominating angle in robotic frame,
   --  return a robotic pose with angle adjusted if discrepancy is below tol.
   --  May raise No_Correction if the angle isn't compatible with tolerances and
   --  guides.

   function Oriented_Pose (Unoriented,
                           Oriented : Pose) return Pose;
   pragma Inline (Oriented_Pose);
   --  return Unoriented with Oriented.Angle

   function To_Radians (Deg : Real) return Angle;

end Sancta.Types.Operations;
