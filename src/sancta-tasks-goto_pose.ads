with Sancta.Tasks.Positioned;
with Sancta.Types;

package Sancta.Tasks.Goto_Pose is

   pragma Preelaborate;

   type Object is new Tasks.Positioned.Object with record
      Use_Angle    : Boolean := False;
      --  Should we take the pose angle when checking arrival?

      Margin_Dist  : Types.Real  := 0.5;
      Margin_Angle : Types.Angle := 0.25;
      --  Leeway for goal reaching.
   end record;

   function Create (Pose      : in Types.Pose;
                    Use_Angle : in Boolean     := True;
                    Margin_D  : in Types.Real  := 0.5;
                    Margin_A  : in Types.Angle := 0.25) return Object;

   function To_String (This : Object) return String;

end Sancta.Tasks.Goto_Pose;
