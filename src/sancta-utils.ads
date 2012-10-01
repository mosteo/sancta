
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Tasks.Containers;
with Sancta.Types;

package Sancta.Utils is

   Criteria_Array : constant array (Enum_Criteria) of Assignment_Criteria :=
     (Minmax => Criterion_Minmax,
      Minmix => Criterion_Minmix,
      Minsum => Criterion_Minsum,
      Minall => Criterion_Minall,
      Minave => Criterion_Minave);

   function Create_Random_Goals
     (Num      : Natural;
      X0, Y0,
      X1, Y1   : Types.Real)
      return Sancta.Tasks.Containers.Lists.List;
   --  Creates tasks located between X0 and X1

end Sancta.Utils;
