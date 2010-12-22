with Sancta.Tasks.Grid_Goal;
use Sancta;

with Agpl.Chronos;
with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Sancta.Mutable_Assignment;
with Agpl.Filesystem;
with Agpl.Generic_File_Store;
with Sancta.Plan;
with Agpl.Optimization.Annealing;
with Agpl.Optimization.Annealing.Solver;
pragma Warnings (Off); with Agpl.Task_Termination; pragma Warnings (On);
with Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io;      use Ada.Text_Io;

--  Annealer

procedure Cell_Anneal is

   package Cost_Matrix_Stores is
     new Agpl.Generic_File_Store (Sancta.Cost_Matrix.Object);
   package Assignment_Stores is
     new Agpl.Generic_File_Store (Sancta.Assignment.Object);

   function "+" (C : in Optimization.Cost) return Sancta.Costs; pragma Inline ("+");
   function "+" (C : in Optimization.Cost) return Sancta.Costs is
      use Optimization;
   begin
      if C = Optimization.Infinite then
         return Sancta.Infinite;
      else
         return Sancta.Costs (C);
      end if;
   end "+";

   package Grid renames Tasks.Grid_Goal;

   use type Sancta.Costs;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line
        ("<map> <Wmm> <Wms> <deadline> " &
         "<incremental> <keep_history> <save-id> [initial ass]");
   end Usage;

   use Assignment_Stores;

   package Solvers is new
     Optimization.Annealing.Solver
       (Sancta.Mutable_Assignment.Object,
        Sancta.Mutable_Assignment.Evaluate,
        Sancta.Mutable_Assignment.Mutate,
        Sancta.Mutable_Assignment.Normalize,
        Sancta.Mutable_Assignment.Last_Mutation,
        Sancta.Mutable_Assignment.Undo);

   Power : constant Float := 1.85; -- Originally 5.0

   Cooling_Top : constant Optimization.Annealing.Temperature := 0.002;

   --  Start with greedy descent
   package Man_Temp is new Optimization.Annealing.Manual_Cooling
     (Initial_Temperature => 0.0,
      Ceiling_Temperature => Cooling_Top,
      Settle_Time         => 60.0,
      Divisor             => Power);

   Anneal    : Sancta.Mutable_Assignment.Object;
   Solver    : Solvers.Object;
   Plan      : Sancta.Plan.Object;
   Criterion : Sancta.Assignment_Criteria;

   Save_Index : Positive := 1;

   Costs : Sancta.Cost_Matrix.Object;
begin
   if Argument_Count < 7 then
      Usage;
      return;
   end if;

   declare
      Mapfile      : String   renames Argument (1);
      Minmax       : Float    renames Float'Value (Argument (2));
      Minsum       : Float    renames Float'Value (Argument (3));
      Deadline     : Duration renames Duration'Value (Argument (4));
      Incremental  : Boolean  renames Boolean'Value (Argument (5));
      Keep_History : Boolean  renames Boolean'Value (Argument (6));
      Save_Id      : String   renames Argument (7);
   begin
      Criterion := (Minmax, Minsum);

      Trace.Add_Tracer (Trace.Console_Tracer);
      Trace.Set_Level (Trace.Debug);
      Trace.Enable_Section (Sancta.Mutable_Assignment.Log_Section);
      --  Trace.Enable_Section (Sancta.Mutable_Assignment.Detail_Section);
      Trace.Enable_Section (Solvers.Log_Section);

      Tasks.Grid_Goal.Parse_Ascii (Mapfile);

      Anneal.Set_Criterion (Criterion);

      -- MUTATIONS --
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Move_Task'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           1.0);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Move_Task_Changing_Owner'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           0.5);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Guided_Move_Task_Changing_Owner'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           1.0);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Auction_Task'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           10.0); -- 3.0
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Guided_Auction_Task'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           2.0); -- 1.0
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Exhaustive_Auction_Task'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           1.0);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Swap_Order'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           3.0);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Swap_Tasks'Access,
                           Sancta.Mutable_Assignment.Undo_Move_Task'Access,
                           0.5);
      Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Agent_Reorder'Access,
                           Sancta.Mutable_Assignment.Undo_From_Scratch'Access,
                           0.001);

      for I in Grid.Agents.First_Index .. Grid.Agents.Last_Index loop
         Anneal.Add_Agent (Grid.Agents.Element (I));
      end loop;
      Put_Line ("Agents added.");

      declare
         use Cost_Matrix_Stores;
         --  We do this here so this cost matrix goes out of scope, saving mem.
      begin
         Load (Costs,
               Filesystem.Replace_Extension (Argument (1), "cost_matrix"));
         Put_Line ("Cost matrix loaded.");
         Anneal.Set_Costs (Costs);
      end;

      for I in Grid.Mission.First_Index .. Grid.Mission.Last_Index loop
         Plan.Add_Task (Grid.Mission.Element (I));
      end loop;
      Put_Line ("Plan created.");

      Anneal.Set_Tasks (Plan);

      if Argument_Count = 8 then
         Put_Line ("Using given initial assignment...");
         declare
            Ini_Ass : Sancta.Assignment.Object;
         begin
            Load (Ini_Ass, Argument (8));
            Anneal.Set_Assignment (Ini_Ass, Criterion);
         end;
      else
         Anneal.Set_Assignment (Anneal.To_Assignment,
                                Criterion);
         --  This awkward call with get the old assignment and create a new
         --  one greedily inserting any new tasks and discarding now
         --  dissapeared tasks.
         Put_Line ("Using greedy insertion for first assignment...");
      end if;

      Solver.Set_Initial_Solution (Anneal);

      --  INITIAL SOLUTION --
      declare
         Assed : constant Sancta.Assignment.Object := Anneal.To_Assignment;
      begin
         To_File (Assed,
                  Filesystem.Replace_Extension
                    (Argument (1), Save_Id & ".ini.ass"));
      end;

      declare
         Timer    : Chronos.Object;
         Second   : Chronos.Object;

         Best_Cost : Sancta.Costs := Anneal.Evaluate;

         --------------
         -- Progress --
         --------------

         Local_Best     : Sancta.Costs := Sancta.Infinite;
         Local_Worst    : Sancta.Costs := 0.0;
         Iters          : Natural  := 0;

         procedure Progress (Continue : out Boolean) is
            use Optimization.Annealing;
         begin
            Iters := Iters + 1;

            declare
               Prev_Temp : constant Optimization.Annealing.Temperature :=
                             Man_Temp.Get_Temperature (0.0);
            begin
               Man_Temp.Update (Solver.Current_Cost);
               if Prev_Temp < Man_Temp.Get_Temperature (0.0) then
                  --  On bump, show stats:
                  Solver.Print_Stats;
                  Solver.Reset_Stats;
               end if;
            end;

            Local_Best  := Sancta.Costs'Min (Local_Best, +Solver.Current_Cost);
            Local_Worst := Sancta.Costs'Max (Local_Worst, +Solver.Current_Cost);

            Continue := Timer.Elapsed <= Deadline;

            if +Solver.Best_Cost < Best_Cost then
               if Incremental then
                  Timer.Reset;
               end if;
               Best_Cost := +Solver.Best_Cost;

               if Keep_History then
                  declare
                     Assed : constant Sancta.Assignment.Object :=
                               Solver.Best_Solution.To_Assignment;
                  begin
                     To_File
                       (Assed,
                        Filesystem.Replace_Extension
                          (Argument (1), Save_Id & "." &
                           To_String (Save_Index) & ".ass"));
                     Save_Index := Save_Index + 1;
                  end;
               else
                  declare
                     Assed : constant Sancta.Assignment.Object :=
                               Solver.Best_Solution.To_Assignment;
                  begin
                     To_File (Assed,
                              Filesystem.Replace_Extension
                                (Argument (1), Save_Id & ".cur.ass"));
                  end;
               end if;

            end if;

            if Second.Elapsed >= 1.0 then
               Put_Line ("REMAINING: " &
                         Duration'Image (Deadline - Timer.Elapsed) &
                         "; Curr cost:" & Sancta.Image (+Solver.Current_Cost) &
                         "; Range:" & Sancta.Image (Local_Best) & "-" &
                         Sancta.Image (Local_Worst) & "; Curr T:" &
                         --  S (Solver.Current_Temperature, 10) &
                         Solver.Current_Temperature'Img &
                         "; I/s:" &
                         Natural'Image (Natural (Float (Iters) / Float (Second.Elapsed))));

               Second.Reset;

               Iters       := 0;
               Local_Best  := Sancta.Infinite;
               Local_Worst := 0.0;
            end if;
         end Progress;

      begin

         Solver.Work (Man_Temp.Get_Temperature'Access,
                      Iterations    => Natural'Last,
                      Timeout       => Duration'Last,
                      Converge      => Deadline,
                      Progress      => Progress'Access,
                      Inform_At_End => True);

      end;

      declare
         Assed : constant Sancta.Assignment.Object :=
           Solver.Best_Solution.To_Assignment;
      begin
         Put_Line ("Assignment completed.");
         To_File (Assed,
                  Filesystem.Replace_Extension
                    (Argument (1), Save_Id & ".fin.ass"));
      end;
   end;

exception
   when E : others =>
      Put_Line (Command_Name & ": " & Trace.Report (E));
end Cell_Anneal;
