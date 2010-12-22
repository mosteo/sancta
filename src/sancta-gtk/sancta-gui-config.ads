with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Sancta.Types;

package Sancta.Gui.Config is

   type Object is record
      Id           : Node_Id;
      Mission_File : Ustring  := +"mission.xml";
      Laser_Scans  : Positive := 4;
      Laser_Range  : Types.Real := 31.0; -- Out-of-range range
      Draw_Grid    : Boolean  := True;
      Show_Poses   : Boolean  := True;
      Show_Mission : Boolean  := False;
   end record;

end Sancta.Gui.Config;
