with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_Io;             use Ada.Text_Io;
with Agpl.Conversions;
with Agpl.Strings;            use Agpl.Strings;
with Agpl.Strings.Fields;     use Agpl.Strings.Fields;
with Agpl.Ustrings;           use Agpl.Ustrings;
with Sancta.Types;            use Sancta.Types;
with Sancta.Types.Operations; use Sancta.Types.Operations;
with Sancta.Types.Real_Math;
                              use Sancta;

procedure Aria2dpslam is
   F : File_Type;
   function S is new Agpl.Conversions.To_Str (Angle);
   function S is new Agpl.Conversions.To_Str (Real);

   Max_Dist : constant Real := Real'Value (Argument (2));
begin
   Open (F, In_File, Argument (1));

   while not End_Of_File (F) loop
      declare
         Line : constant String := Get_Line (F);
         Tag  : constant String := Head (Line, ':');
         Data : constant String := Trim (Crunch (Tail (Line, ':')));
         Pos  :          Pose;
      begin
         if Tag = "robot" then
            Pos.X := Real'Value (Select_Field (Data, 1)) / 1000.0;
            Pos.Y := Real'Value (Select_Field (Data, 2)) / 1000.0;
            Pos.A := To_Radians (Real'Value (Select_Field (Data, 3)));
            Put_Line ("Odometry " &
                      S (Pos.X, 5) & " " &
                      S (Pos.Y, 5) & " " &
                      S (Pos.A, 5));
         elsif Tag = "scan1" then
            declare
               Idx  : Positive := 1;
               D    : Real;
               X, Y : Real;
               Str  : Ustring := +"Laser ";
            begin
               Asu.Append (Str, To_String (Num_Tokens (Data) / 2));
               for I in 1 .. Num_Tokens (Data) / 2 loop
                  X := Real'Value (Select_Field (Data, Idx));
                  Y := Real'Value (Select_Field (Data, Idx + 1));
                  Idx := Idx + 2;
                  D := Real_Math.Sqrt (X * X + Y * Y) / 1000.0;
                  if D > Max_Dist then
                     D := 0.0;
                  end if;

                  Asu.Append (Str, " " & S (D, 5));
               end loop;
               Put_Line (+Str);
            end;
         end if;
      end;
   end loop;

   Close (F);

end Aria2dpslam;
