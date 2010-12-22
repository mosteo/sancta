with Sancta.Tasks.Grid_Goal;
with Sancta.Types; use Sancta.Types;
use Sancta;

with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

procedure Cell_Random_Dot is

   procedure Usage is
   begin
      Put_Line ("<# places>");
   end Usage;

   package Grid renames Tasks.Grid_Goal;

   Places : Positive;
begin
   if Argument_Count /= 1 then
      Usage;
      return;
   end if;

   Places := Positive'Value (Argument (1));

   declare
      Poses     : Sancta.Types.Pose_Array (1 .. Places);
   begin
      loop
         Grid.Clear;

         --  Create places:
         for I in Poses'Range loop
            Poses (I).X := Real (Random.Get_Float (0.0, 100.0));
            Poses (I).Y := Real (Random.Get_Float (0.0, 100.0));
            Poses (I).A := 0.0;
         end loop;

         --  Create graph:
         Grid.Create_From_Poses (Poses, 20.0);

         exit when Grid.Graph.Is_Connected;
      end loop;
      Put_Line (+Grid.To_Dot);
   end;

exception
   when E : others =>
      Put_Line (Standard_Error, "Main: " & Report (E));
end Cell_Random_Dot;
