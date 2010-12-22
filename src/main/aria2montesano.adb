with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_Io;             use Ada.Text_Io;
with Agpl.Conversions;
with Agpl.Strings;            use Agpl.Strings;
with Agpl.Strings.Fields;     use Agpl.Strings.Fields;
with Sancta.Types;            use Sancta.Types;
with Sancta.Types.Operations; use Sancta.Types.Operations;
with Sancta.Types.Real_Math;
                              use Sancta;

procedure Aria2montesano is
   F : File_Type;
   function S is new Agpl.Conversions.To_Str (Real);
begin
   Open (F, In_File, Argument (1));

   Put_Line ("# From aria");

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
         elsif Tag = "scan1" then
            declare
               Idx  : Positive := 1;
               D    : Real;
               X, Y : Real;
            begin
               for I in 1 .. Num_Tokens (Data) / 2 loop
                  X := Real'Value (Select_Field (Data, Idx));
                  Y := Real'Value (Select_Field (Data, Idx + 1));
                  Idx := Idx + 2;
                  D := Real_Math.Sqrt (X * X + Y * Y) / 1000.0;
                  Put_Line (S (D, 5));
               end loop;
               Put_Line (Image (Pos, 5));
            end;
         end if;
      end;
   end loop;

   Close (F);

end Aria2montesano;
