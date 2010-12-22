with Sancta.Tasks.Goto_Pose;

with Agpl.Random;
use Agpl;

package body Sancta.Utils is

   package Tc renames Sancta.Tasks.Containers;

   use Tasks;
   use type Types.Real;

   -------------------------
   -- Create_Random_Goals --
   -------------------------

   function Create_Random_Goals
     (Num      : Natural;
      X0, Y0,
      X1, Y1   : Types.Real)
      return Sancta.Tasks.Containers.Lists.List
   is
      Result : Tc.Lists.List;
   begin
      while Natural (Result.Length) /= Num loop
         declare
            Goal    : constant Goto_Pose.Object :=
                        Goto_Pose.Create
                          ((+Random.Get_Float (+X0, +X1),
                            +Random.Get_Float (+Y0, +Y1),
                            0.0));
         begin
            Result.Append (Goal);
         end;
      end loop;
      return Result;
   end Create_Random_Goals;

end Sancta.Utils;
