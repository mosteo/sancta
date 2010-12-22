

--  Root package for Cooperative Robotics

--  with Agpl.Optimization;

package Sancta.Criteria is

   pragma Preelaborate;

   type Assignment_Criteria is record
      Minmax_Weight : Float := 0.0;
      Minsum_Weight : Float := 0.0;
      Minavg_Weight : Float := 0.0;
   end record;
   --  Possibilities for assignments
   --  Defaults to invalid to ensure that we set it!

   function Evaluate (Criterion : in Assignment_Criteria;
                      Minmax    : in Costs;
                      Minsum    : in Costs;
                      Minavg    : in Costs) return Costs;
   pragma Inline (Evaluate);

   function Image (C : in Costs) return String;
   pragma Inline (Image);

   function Image (C : in Costs; Decimals : in Natural) return String;
   pragma Inline (Image);

   function Value (S : in String) return Assignment_Criteria;
   --  "minmax minsum minavg"

   Criterion_Invalid       : constant Assignment_Criteria := (0.0, 0.0, 0.0);
   Criterion_Minmax        : constant Assignment_Criteria := (1.0, 0.0, 0.0);
   Criterion_Minsum        : constant Assignment_Criteria := (0.0, 1.0, 0.0);
   Criterion_Minmix        : constant Assignment_Criteria := (1.0, 1.0, 0.0);
   Criterion_Minall        : constant Assignment_Criteria := (1.0, 1.0, 1.0);
   Criterion_Mintim        : constant Assignment_Criteria := (1.0, 0.00001, 0.0);
   Criterion_Minavg        : constant Assignment_Criteria := (0.0, 0.0, 1.0);

   Criterion_Minimax       : Assignment_Criteria renames Criterion_Minmax;
   Criterion_Totalsum      : Assignment_Criteria renames Criterion_Minsum;
   Criterion_Time_Critical : Assignment_Criteria renames Criterion_Minmix;
   Criterion_Best          : Assignment_Criteria renames Criterion_Minmix;

   type Enum_Criteria is (Minmax, Minmix, Minsum, Minall, Minave);

end Sancta.Criteria;
