with Sancta.Debug.Parse_Montesano; use Sancta.Debug;
with Sancta.Draw;
with Sancta.Types.Operations; use Sancta.Types.Operations;
with Sancta.Types.Transformations; use Sancta.Types.Transformations;
use  Sancta.Types.Transformations.Real_Transf;
with Sancta.Types;   use Sancta.Types;

with MbIcp;

with Agpl.Trace; use Agpl.Trace;

with Text_Io; use Text_Io;
with Ada.Command_Line; use Ada.Command_Line;

--  1st argument : log file
--  2nd argument : Readings per scan
--  3rd argument : Max_range

procedure Sancta.Main.Sm_Plot is
begin
   declare

      package Sm renames Mbicp;
      subtype Smf is Sm.Mbicp_Float;

      Max_Range  : constant Real := Real'Value (Argument (3));

      Orig_Scans : constant Types.Posed_Range_Scan_Vectors.Vector :=
                     Parse_Montesano (Argument (1),
                                      Natural'Value (Argument (2)));
      All_Scans  : Types.Posed_Range_Scan_Vectors.Vector;

      ----------------
      -- To_Sm_Scan --
      ----------------

      function To_Sm_Scan (X : in Types.Range_Scan)
                        return Sm.Laser_Scan
      is
         Result : Sm.Laser_Scan (1 .. X'Length);
      begin
         for I in Result'Range loop
            Result (I).Bearing  := Smf (X (I).A);
            Result (I).Distance := Smf (X (I).D);
         end loop;
         return Result;
      end To_Sm_Scan;

      Iters             : Natural := 0;

      Smprv_Pose : Types.Pose := Types.Origin;

   begin
      Sm.Init (Bw => 1.57 / 2.0);
      Sm.Set_Out_Of_Range (Smf (Max_Range));

      --  Now adjust the pose via scan matching
      for I in Orig_Scans.First_Index + 1 .. Orig_Scans.Last_Index loop
         Iters := Iters + 1;
         declare
            Curr      : Posed_Range_Scan renames Orig_Scans.Element (I);
            Prev      : Posed_Range_Scan renames Orig_Scans.Element (I - 1);
            Od_Delta  : constant Types.Pose := +Decompose (+Prev.Pose, +Curr.Pose);
            Sm_Delta  : Sm.Pose;
            Outcome   : Sm.Outcomes;
         begin
            Sm.Match (To_Sm_Scan (Prev.Scan),
                      To_Sm_Scan (Curr.Scan),
                      (Smf (Od_Delta.X), Smf (Od_Delta.Y), Smf (Od_Delta.A)),
                      Sm_Delta,
                      Outcome);

            if Outcome in Sm.Outcomes_Ok then
               declare
                  Sm_Result : constant Types.Pose := Types.Pose'(Real (Sm_Delta.X),
                                                                 Real (Sm_Delta.Y),
                                                                 To_Angle
                                                                   (Real (Sm_Delta.A)));
                  M         : constant Real := Modulus (Sm_Result);
               begin
                  Put_Line ("Odom: " & To_String (Od_Delta) &
                            "; SM: " & To_String (Sm_Result));
                  Put_Line (Iters'Img & " " & Outcome'Img & " Mod: " & To_String (M));
                  if M > 1.0 then
                     Put_Line ("Warning: Jump???");
                     delay 1.0;
                  end if;

                  declare
                     New_Scan : Posed_Range_Scan := Curr;
                  begin
                     New_Scan.Pose := +Compose (+Smprv_Pose, +Sm_Result);
                     All_Scans.Append (New_Scan);
                     Smprv_Pose    := New_Scan.Pose;
                  end;
               end;
            else
               Put_Line (Iters'Img & " " & Outcome'Img);
               delay 1.0;
            end if;
         end;
      end loop;

      Sancta.Draw.Draw_Laser (All_Scans, Max_Range);

   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
      Put_Line ("Arguments: <logfile> <reads per scan> <max range>");
end Sancta.Main.Sm_Plot;
