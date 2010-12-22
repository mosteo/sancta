with Sancta.Tasks.Grid_Goal;
use Sancta;

with Sancta.Assignment;
with Agpl.Generic_File_Store;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Cell_Dot_Assignment is

   package Grid renames Tasks.Grid_Goal;

   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);
   use Assignment_Stores;

   procedure Usage is
   begin
      Put_Line ("<floorplan> <ass>");
   end Usage;

begin
   if Argument_Count /= 2 then
      Usage;
      return;
   end if;

   Grid.Parse_Ascii (Argument (1));

   declare
      Ass   : Sancta.Assignment.Object;
   begin
      Load (Ass, Argument (2));

      Put_Line (+Grid.To_Dot (Ass));
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Cell_Dot_Assignment;
