with Sancta.Tasks.Goto_Pose,
     Sancta.Types;

use Sancta;

package Sancta.Ctree.Tasks.Go is

   pragma Preelaborate;

   type Fwrd is new Sancta.Tasks.Goto_Pose.Object with null record;

   function Go_Fwrd (Pose      : in Types.Pose;
                     Use_Angle : in Boolean     := True;
                     Margin_D  : in Types.Real  := 0.5;
                     Margin_A  : in Types.Angle := 0.25) return Fwrd;

   type Back is new Sancta.Tasks.Goto_Pose.Object with null record;

   function Go_Back (Pose      : in Types.Pose;
                     Use_Angle : in Boolean     := True;
                     Margin_D  : in Types.Real  := 0.5;
                     Margin_A  : in Types.Angle := 0.25) return Back;

end Sancta.Ctree.Tasks.Go;
