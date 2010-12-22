with Sancta.Tasks.Grid_Goal;

with Agpl.Filesystem;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

--  Computes the assignment using the Iterated Assignation Problem.
--  2-competitive

procedure Sancta.Main.Cell_Iap is

   procedure Usage is
   begin
      Put_Line ("First argument must be a file to parse");
   end Usage;

begin
   if Argument_Count /= 1 then
      Usage;
      return;
   end if;

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Sancta.Main.Cell_Iap;
