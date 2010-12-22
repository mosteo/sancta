with Sancta.Tasks.Grid_Goal;
use Sancta;

with Agpl.Conversions;
with Sancta.Assignment;
with Agpl.Generic_File_Store;
with Sancta.Tasks;
with Agpl.Random;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Cell_Bomb is

   --  Place N times a bomb in a task

   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);
   use Assignment_Stores;

   package Grid renames Tasks.Grid_Goal;

   function S is new Conversions.To_Str (Sancta.Costs);

   procedure Usage is
   begin
      Put_Line ("<floorplan> <ass> <iterations> <results> <wmm> <wms>");
   end Usage;

   Criterion : Sancta.Assignment_Criteria;
begin
   if Argument_Count /= 6 then
      Usage;
      return;
   end if;

   Criterion := (Minmax_Weight => Float'Value (Argument (5)),
                 Minsum_Weight => Float'Value (Argument (6)));

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   declare
      Ass   : Sancta.Assignment.Object;
      F     : File_Type;
   begin
      Load (Ass, Argument (2));
      Put_Line ("Assignment loaded.");

      Ass.Print_Summary;

      Create (F, Name => Argument (4), Mode => Out_File);

      for I in 1 .. Positive'Value (Argument (3)) loop
         declare
            Bomb_At : constant Sancta.Tasks.Task_Id :=
                        Grid.Mission.Element
                          (Random.Get_Integer
                             (Grid.Mission.First_Index,
                              Grid.Mission.Last_Index)).Get_Id;
         begin
            Put_Line (I'Img & ":" & S (Ass.Get_Cost_Until_Task (Bomb_At, Criterion)));
            Put_Line (F, S (Ass.Get_Cost_Until_Task (Bomb_At, Criterion)));
         end;
      end loop;
      Close (F);
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Cell_Bomb;
