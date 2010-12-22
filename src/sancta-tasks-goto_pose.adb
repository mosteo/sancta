with Sancta.Debug;

package body Sancta.Tasks.Goto_Pose is

   ------------
   -- Create --
   ------------

   function Create (Pose      : in Types.Pose;
                    Use_Angle : in Boolean     := True;
                    Margin_D  : in Types.Real  := 0.5;
                    Margin_A  : in Types.Angle := 0.25) return Object
   is
      Result : Object;
   begin
      Result.Pose      := Pose;
      Result.Use_Angle := Use_Angle;
      if not Use_Angle then
         Result.Pose.A := 0.0;
      end if;

      Result.Margin_Dist  := Margin_D;
      Result.Margin_Angle := Margin_A;

      return Result;
   end Create;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
      use Debug;
   begin
      if This.Use_Angle then
         return "Go To " & To_String (This.Pose);
      else
         return "Go To (" & To_String (This.Pose.X) & "," &
                            To_String (This.Pose.Y) & ")";
      end if;
   end To_String;

end Sancta.Tasks.Goto_Pose;
