with Agpl.Strings.Fields;

package body Sancta.Convert is

   -------------
   -- To_Pose --
   -------------

   function To_Pose (S : in String) return Types.Pose is
      use Agpl.Strings.Fields;
      Result : Types.Pose;
   begin
      if S (S'First) = '(' then
         pragma Unimplemented;
         raise Program_Error; -- Not implemented;
      else
         Result.X := Types.Real'Value (Select_Field (S, 1));
         Result.Y := Types.Real'Value (Select_Field (S, 2));
         Result.A := Types.To_Angle (Types.Real'Value (Select_Field (S, 3)));
      end if;

      return Result;
   end To_Pose;

end Sancta.Convert;
