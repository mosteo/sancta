with Sancta.Tasks.Containers;
with Sancta.Tasks;
with Sancta.Types;

package Sancta.Tasks.Creation is

   --  pragma Preelaborate;

   function Create_Random_Goals
     (Num      : Natural;
      X0, Y0,
      X1, Y1   : Types.Real)
      return Sancta.Tasks.Containers.Lists.List;
   --  Creates tasks located between X0 and X1

end Sancta.Tasks.Creation;
