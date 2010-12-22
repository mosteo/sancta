with Sancta.Tasks.Grid_Goal;
with Sancta.Traderbot_Sim;
use Sancta;

with Agpl.Chronos;
with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Agpl.Filesystem;
with Agpl.Generic_File_Store;
with Sancta.Tasks.Containers;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Random;
with Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

--  Traderbot sim with timed period for auctions and random starting order

procedure Cell_Timed_Traderbot is

   package Cost_Matrix_Stores is
     new Agpl.Generic_File_Store (Sancta.Cost_Matrix.Object);
   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);

   package Grid renames Tasks.Grid_Goal;

   procedure Usage is
   begin
      Put_Line ("<map> <Wminmax> <Wminsum> <deadline> <keep_history> <save-id>");
   end Usage;

   use Assignment_Stores;

   Keep_History : Boolean;

begin
   if Argument_Count /= 6 then
      Usage;
      return;
   end if;

   Keep_History := Boolean'Value (Argument (5));

   Tasks.Grid_Goal.Parse_Ascii (Argument (1));

   declare
      Save_Id : String renames Argument (6);
      Asser   : Traderbot_Sim.Object;
   begin
      declare
         Costs : Sancta.Cost_Matrix.Object;
         use Cost_Matrix_Stores;
         --  We do this here so this cost matrix goes out of scope, saving mem.
      begin
         Load (Costs,
               Filesystem.Replace_Extension (Argument (1), "cost_matrix"));
         Asser.Set_Costs (Costs);
      end;

      Asser.Set_Criterion ((Minmax_Weight => Float'Value (Argument (2)),
                            Minsum_Weight => Float'Value (Argument (3)),
                            Minavg_Weight => 0.0));
      Put_Line ("Cost matrix loaded.");

      for I in Grid.Agents.First_Index .. Grid.Agents.Last_Index loop
         Asser.Add_Agent (Grid.Agents.Element (I));
      end loop;
      Put_Line ("Agents added.");

      declare
         Rand    : Random.Object;
         Mission : Sancta.Tasks.Containers.Vectors.Vector := Grid.Mission;
      begin
         Rand.Reset;
         while not Mission.Is_Empty loop
            declare
               Chosen : constant Positive :=
                          Rand.Get_Integer (1, Integer (Mission.Length));
            begin
               Asser.Add_Task (Mission.Element (Chosen));
               Mission.Delete (Chosen);
               Put (".");
            end;
         end loop;
         New_Line;
      end;

      declare
         Assed : constant Sancta.Assignment.Object := Asser.To_Assignment;
      begin
         To_File (Assed,
                  Filesystem.Replace_Extension
                    (Argument (1), Save_Id & "_ini.ass"));
      end;

      declare
         Deadline : constant Duration := Duration'Value (Argument (4));
         Timer    : Chronos.Object;
         Second   : Chronos.Object;
         Index    : Positive := 1;
         Iters    : Natural  := 0;
      begin
         while Timer.Elapsed <= Deadline loop
            declare
               Changes : Traderbot_Sim.Outcomes;
               use Traderbot_Sim;
            begin
               Asser.Auction_Random_Task (Changes);
               Iters := Iters + 1;
               if Changes = Change_Improve then
                  if Keep_History then
                     declare
                        Assed : constant Sancta.Assignment.Object :=
                                  Asser.To_Assignment;
                     begin
                        To_File
                          (Assed,
                           Filesystem.Replace_Extension
                             (Argument (1),
                              Save_Id & "." & To_String (Index) & ".ass"));
                     end;
                     Index := Index + 1;
                  end if;
               end if;

               if Second.Elapsed >= 5.0 then
                  Second.Reset;
                  Put_Line ("REMAINING: " &
                            Duration'Image (Deadline - Timer.Elapsed) &
                            " (" & To_String (Iters) & " a/s)");
                  Iters := 0;
               end if;
            end;
         end loop;
      end;

      declare
         Assed : constant Sancta.Assignment.Object := Asser.To_Assignment;
      begin
         Put_Line ("Assignment completed.");
         To_File (Assed,
                  Filesystem.Replace_Extension
                    (Argument (1), Save_Id & "_fin.ass"));
         --  Assed.Print_Assignment;
      end;
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Trace.Report (E));
end Cell_Timed_Traderbot;
