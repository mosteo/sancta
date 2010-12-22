with Sancta.Tasks.Grid_Goal;
use Sancta;

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Agpl.Generic_File_Store;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Agpl.Random;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Text_Io;      use Ada.Text_Io;

procedure Cell_Nbombs is

   --  Find at most N bombs before a given deadline

   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);
   use Assignment_Stores;

   package Task_Lists renames Sancta.Tasks.Containers.Lists;
   package Agent_Lists renames Sancta.Agent.Containers.Lists;

   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;

   package Id_Sets is new
     Ada.Containers.Ordered_Sets (Sancta.Tasks.Task_Id);

   package Grid renames Tasks.Grid_Goal;

   procedure Usage is
   begin
      Put_Line ("<floorplan> <ass> <iterations> <results> <bombs> <deadline>");
   end Usage;
begin
   if Argument_Count /= 6 then
      Usage;
      return;
   end if;

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   declare
      Ass      : Sancta.Assignment.Object;
      F        : File_Type;
      Deadline : constant Sancta.Costs := Sancta.Costs'Value (Argument (6));
   begin
      Load (Ass, Argument (2));
      Put_Line ("Assignment loaded.");

      Ass.Print_Summary;

      Create (F, Name => Argument (4), Mode => Out_File);

      for I in 1 .. Positive'Value (Argument (3)) loop
         declare
            Bombs : Id_Sets.Set;
            Found : Natural := 0;
            use Id_Sets;

            procedure Create_Bombs is
               Pending : Natural := Natural'Value (Argument (5));
               Id      : Sancta.Tasks.Task_Id;
            begin
               while Pending > 0 loop
                  Id :=
                    Grid.Mission.Element
                      (Random.Get_Integer
                           (Grid.Mission.First_Index,
                            Grid.Mission.Last_Index)).Get_Id;
                  if not Bombs.Contains (Id) then
                     Bombs.Insert (Id);
                     Pending := Pending - 1;
                  end if;
               end loop;
            end Create_Bombs;

            procedure Find_Bombs is
               Agents : constant Agent_Lists.List := Ass.Get_Agents;

               procedure Check_Agent (I : Agent_Lists.Cursor) is
                  use Task_Lists;
                  Ag    : constant Sancta.Agent.Object'Class :=
                            Agent_Lists.Element (I);
                  Tasks : constant Task_Lists.List := Ag.Get_Tasks;
                  Acum  : Sancta.Costs := 0.0;
                  T     : Task_Lists.Cursor := Tasks.First;
               begin
                  while Has_Element (T) loop
                     if T /= Tasks.First then
                        Acum := Acum + Ag.Get_Cost (Element (Previous (T)),
                                                    Element (T));
                     else
                        Acum := Acum + Ag.Get_Cost (Element (T));
                     end if;

                     exit when Acum > Deadline;

                     if Bombs.Contains (Element (T).Get_Id) then
                        Found := Found + 1;
                     end if;

                     Next (T);
                  end loop;
               end Check_Agent;
            begin
               Agents.Iterate (Check_Agent'Access);
            end Find_Bombs;

         begin
            Create_Bombs;
            Find_Bombs;

            Put_Line (I'Img & ":" & Found'Img);
            Put_Line (F, Found'Img);
         end;
      end loop;
      Close (F);
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Report (E));
end Cell_Nbombs;
