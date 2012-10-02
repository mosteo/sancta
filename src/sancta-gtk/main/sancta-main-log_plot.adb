with Sancta.Debug.Parse_Montesano; use Sancta.Debug;
--  with Sancta.Draw;
with Sancta.Types.Transformations; use Sancta.Types.Transformations;
use  Sancta.Types.Transformations.Real_Transf;
with Sancta.Types;   use Sancta.Types;

with Agpl.Trace; use Agpl.Trace;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

--  1st argument : log file
--  2nd argument : Readings per scan
--  3rd argument : Max_range

procedure Sancta.Main.Log_Plot is

begin
   declare
      Max_Range : constant Real := Real'Value (Argument (3));

      All_Scans         : constant Types.Posed_Range_Scan_Vectors.Vector :=
                            Parse_Montesano (Argument (1),
                                             Natural'Value (Argument (2)));
   begin
      --  Sancta.Draw.Draw_Laser (All_Scans, Max_Range);
      Put_Line ("Laser drawing is unavailable because of bit rot");
   end;
exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
      Put_Line ("Arguments: <logfile> <reads per scan> <max range>");
end Sancta.Main.Log_Plot;
