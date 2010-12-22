with Sancta.Draw;
with Sancta.Tasks.Grid_Goal;

with Sancta.Assignment;
with Agpl.Gdk.Managed;
with Agpl.Generic_File_Store;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Sancta.Main.Show_Assignment is

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

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   declare
      Ass   : Sancta.Assignment.Object;
   begin
      Load (Ass, Argument (2));
      Put_Line ("Assignment loaded.");

      Agpl.Gdk.Managed.Start;
      Draw.Draw_Assignment (Ass, Show_Costs => True);
      Ass.Print_Assignment;
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Sancta.Main.Show_Assignment;
