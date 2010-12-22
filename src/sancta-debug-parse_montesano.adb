with Sancta.Convert;

with Agpl.Trace;   use Agpl.Trace;

with Ada.Text_Io;

function Sancta.Debug.Parse_Montesano
  (Filename : in String;
   Rps      : in Natural)
   return        Types.Posed_Range_Scan_Vectors.Vector
is
   use Ada.Text_Io;
   use Types;
   F                 : File_Type;
   Readings_Per_Scan : Natural renames Rps;

   All_Scans         : Types.Posed_Range_Scan_Vectors.Vector;
   Read              : Natural := 0;
begin
   Open (F, In_File, Filename);

   Skip_Line (F);

   begin
      while not End_Of_File (F) loop
         declare
            A    : Angle := -3.1415926535 / 2.0;
            Step : constant Angle := 3.1415926535 / Angle (Readings_Per_Scan);
            Scan : Posed_Range_Scan (Readings_Per_Scan);
         begin
            for I in Scan.Scan'Range loop
               declare
                  Line : constant String := Get_Line (F);
               begin
--                    Put_Line (Line);
                  Scan.Scan (I).A := A;
                  Scan.Scan (I).D := Real'Value (Line);
                  A := A + Step;
               end;
            end loop;

            Scan.Pose := Convert.To_Pose (Get_Line (F));

            All_Scans.Append (Scan);
            Read := Read + 1;
            Put (".");
         end;
      end loop;
   exception
      when E : others =>
         Close (F);
         Log ("Parse_Montesano: " & Report (E), Error);
         return All_Scans;
   end;
   if Is_Open (F) then
      Close (F);
   end if;
   New_Line;
   return All_Scans;
end Sancta.Debug.Parse_Montesano;
