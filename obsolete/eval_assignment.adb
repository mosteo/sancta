with Sancta.Tasks.Grid_Goal;
use  Sancta;

with Agpl.Conversions;
with Sancta.Assignment;
with Agpl.Generic_File_Store;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Eval_Assignment is

   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);
   use Assignment_Stores;

   procedure Usage is
   begin
      Put_Line (Standard_Error, "<floorplan> <ass> <Wmm> <Wms>");
   end Usage;

   function S is new Conversions.To_Str (Sancta.Costs);

begin
   if Argument_Count /= 4 then
      Usage;
      return;
   end if;

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   declare
      Ass   : Sancta.Assignment.Object;
   begin
      Load (Ass, Argument (2));
      Put_Line
        (S
           (Ass.Get_Cost
              ((Float'Value (Argument (3)), Float'Value (Argument (4)))), 9));
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Eval_Assignment;
