with Sancta.Tasks.Grid_Goal;

with Sancta.Agent.Utils; use Sancta.Agent.Utils;
with Sancta.Cost_Matrix;
with Agpl.Filesystem;
with Agpl.Generic_File_Store;
with Sancta.Tasks.Utils; use Sancta.Tasks.Utils;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

with Gnat.Os_Lib; use Gnat.Os_Lib;

procedure Sancta.Main.Cell_Create_Costs is

   package Cost_Matrix_Stores is
     new Agpl.Generic_File_Store (Sancta.Cost_Matrix.Object);

   procedure Usage is
   begin
      Put_Line ("<map> <noisify:bool>");
   end Usage;

begin
   if Argument_Count /= 2 then
      Usage;
      return;
   end if;

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   Tasks.Grid_Goal.Noisify := Boolean'Value (Argument (2));

   if not Is_Regular_File (Filesystem.Replace_Extension (Argument (1), "gcosts")) then
      Tasks.Grid_Goal.Cost_Store.To_File
        (Tasks.Grid_Goal.Graph.Get_Costs,
         Filesystem.Replace_Extension (Argument (1), "gcosts"));
      Put_Line ("Done [Graph costs].");
   else
      Put_Line ("Graph costs already present, nothing to do.");
   end if;

   declare
      Costs : constant Sancta.Cost_Matrix.Object :=
                Sancta.Cost_Matrix.Create_With_Start
                  (To_List (Tasks.Grid_Goal.Agents),
                   To_List (Tasks.Grid_Goal.Mission));
      use Cost_Matrix_Stores;
   begin
      if not Is_Regular_File
        (Filesystem.Replace_Extension (Argument (1), "cost_matrix"))
      then
         To_File (Costs,
                  Filesystem.Replace_Extension (Argument (1), "cost_matrix"));
         Put_Line ("Done [Matrix cost].");
      else
         Put_Line ("Cost matrix already present, nothing to do.");
      end if;
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Sancta.Main.Cell_Create_Costs;
