with Sancta.Tasks.Grid_Goal;
use Sancta;

with Sancta.Assigner.Mtsp_Concordefake;
use  Sancta.Assigner.Mtsp_Concordefake;
--  Replace two above lines with the assigner of interest...

with Sancta.Agent.Utils; use Sancta.Agent.Utils;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Agpl.Filesystem;
with Agpl.Generic_File_Store;
with Sancta.Tasks.Utils; use Sancta.Tasks.Utils;
with Agpl.Optimization.Concordefake;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Cell_Concordefake is

   package Cost_Matrix_Stores is
     new Agpl.Generic_File_Store (Sancta.Cost_Matrix.Object);
   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);

   procedure Usage is
   begin
      Put_Line ("<map> <.sol>");
   end Usage;

   task Main is
      pragma Storage_Size (10_240 * 1_024);
   end Main;

   task body Main is
   begin
      if Argument_Count /= 2 then
         Usage;
         abort Main;
      end if;

      Tasks.Grid_Goal.Parse_Ascii (Argument (1));

      Optimization.Concordefake.Solution_File := +Argument (2);

      declare
         Asser : Object;
         Costs : Sancta.Cost_Matrix.Object;
         use Cost_Matrix_Stores;
      begin
         --        Asser.Criterion.Minmax_Weight := Float'Value (Argument (2));
         --        Asser.Criterion.Minsum_Weight := Float'Value (Argument (3));
         Costs := Load (Filesystem.Replace_Extension (Argument (1), "cost_matrix"));
         Put_Line ("Cost matrix loaded.");

         declare
            Assed : constant Sancta.Assignment.Object :=
                      Asser.Assign (To_List (Tasks.Grid_Goal.Agents),
                                    To_List (Tasks.Grid_Goal.Mission),
                                    Costs);
            use Assignment_Stores;
         begin
            Put_Line ("Assignment completed.");
            To_File (Assed,
                     Filesystem.Replace_Extension
                       (Argument (1), Object'External_Tag & ".ass"));
            Assed.Print_Assignment;
         end;
      end;
   exception
      when E : others =>
         Put_Line (Command_Name & ": " & Trace.Report (E));
   end Main;

begin
   null;
exception
   when E : others =>
      Put_Line (Command_Name & ": " & Trace.Report (E));
end Cell_Concordefake;
