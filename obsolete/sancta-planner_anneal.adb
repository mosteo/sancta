--  with Sancta.Draw;
with Sancta.Mutable_Assignment;

with Agpl.Chronos;
with Sancta.Assigner;
with Sancta.Assigner.Hungry3;
with Sancta.Cost_Matrix;
with Sancta.Plan_Node;
with Sancta.Plan.Utils.Random;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Optimization.Annealing.Solver;
with Agpl.Strings;
with Agpl.Trace;

with Ada.Calendar; use Ada.Calendar;

package body Sancta.Planner_Anneal is

   use Agpl;
   use Agpl.Trace;

   Offline_Period : constant Duration := 60.0;
   Time_Speedup   : constant Duration :=  1.0;

   Known_Optimal_Solution : constant Optimization.Cost := 0.0; -- 1274.69;

   Show_Partial_Solutions : constant Boolean := False;

   package Solver_Minimax is new Optimization.Annealing.Solver
     (Mutable_Assignment.Object,
      Mutable_Assignment.Evaluate_Minimax,
      Mutable_Assignment.Mutate,
      Mutable_Assignment.Normalize,
      Mutable_Assignment.Last_Mutation,
      Mutable_Assignment.Is_Valid);

   package Solver_Totalsum is new Optimization.Annealing.Solver
     (Mutable_Assignment.Object,
      Mutable_Assignment.Evaluate_Totalsum,
      Mutable_Assignment.Mutate,
      Mutable_Assignment.Normalize,
      Mutable_Assignment.Last_Mutation,
      Mutable_Assignment.Is_Valid);

   ------------
   -- Replan --
   ------------

   procedure Replan (This : in out Object) is
      Pending_Tasks : constant Sancta.Tasks.Containers.Lists.List := This.Get_Pending_Tasks;

      Plan          :          Sancta.Plan.Object      := Get_Planner (This);

--        function Cooler is
--          new Optimization.Annealing.Proportional_Cooling
--            (Factor => 0.9998,
--             Umbral => 0.001);

      function Cooler is
        new Optimization.Annealing.Cyclic_Cooling
          (Start  => Clock,
           Period => 20.0,
           Power  => 5.0); -- Original: 4.0
   begin
      --  Prepare an unexpanded plan:
      Sancta.Plan.Add_Subplan (Plan,
                            Sancta.Plan_Node.Create (Sancta.Plan_Node.And_Node,
                                                  Pending_Tasks));

--      Sancta.Plan.Print_Tree_Summary (Plan);

      --  Already inflated so we can get a random solution
      Plan := Sancta.Plan.Inflate (Plan);

      declare
         Solution : Mutable_Assignment.Object :=
                      Mutable_Assignment.To_Mutable (This.Get_Alive_Agents,
                                                     Plan);
         --  Make a default assignment:
         Prim_Tasks    : constant Sancta.Tasks.Containers.Lists.List :=
                           Sancta.Plan.Enumerate_Tasks
                             (Sancta.Plan.Utils.Get_Any_Expansion (Plan),
                              Primitive => True,
                              Pending   => True);

         Costs         : constant Sancta.Cost_Matrix.Object :=
                           Sancta.Cost_Matrix.Create_With_Start
                             (Get_Alive_Agents (This), Prim_Tasks);

         Assignation   : constant Sancta.Assignment.Object  :=
                           Sancta.Assigner.Hungry3.Assign
                             ((Sancta.Assigner.Object with Keep_Order => False),
                              Get_Alive_Agents (This),
                              Prim_Tasks,
                              Costs);
      begin
         Log ("Planning" & Pending_Tasks.Length'Img & " tasks " &
              "(" & Strings.To_String (Natural (Prim_Tasks.Length)) &
              " primitive)", Always);

--         Sancta.Plan.Print_Tree_Summary (Plan);
--         Sancta.Assignment.Print_Assignment (Assignation);
--           Log ("Assignment minimax cost is " &
--                Strings.To_String (Float (Assignation.Get_Max_Min_Cost)), Always);

         --  Update the Solution with this initial assignment
         Mutable_Assignment.Update (Solution, Assignation);

         case This.Criterion is
         when Sancta.Minimax =>
            declare
               Solver    : Solver_Minimax.Object;
               Best_Cost : Optimization.Annealing.Cost :=
                             Optimization.Annealing.Cost'Last;
               Cron      : Chronos.Object;
               Begun     : Boolean := False;

               --------------
               -- Progress --
               --------------

               procedure Progress (Continue : out Boolean) is
                  use Optimization.Annealing;
                  Curr_Cost : constant Cost := Solver.Best_Cost;
                  Best_Sol  : Mutable_Assignment.Object := Solver.Best_Solution;
                  Changes   : Boolean;
               begin
                  if Curr_Cost < Best_Cost then
                     Best_Cost := Curr_Cost;
                     if Show_Partial_Solutions then
                        Draw.Draw_Assignment
                          (Solver.Best_Solution.To_Assignment);
                        --  Solver.Best_Solution.To_Assignment.Print_Assignment;
                        Send_Plan_To_Agents
                          (This, Mutable_Assignment.To_Assignment (Solution));
                     end if;
                  end if;

                  --  So tasks are frozen
                  if Best_Cost <= Known_Optimal_Solution then
                     Best_Sol.Mark_Elapsed (10000.0, Changes); -- Best sol, ending
                  else
                     Best_Sol.Mark_Elapsed
                       ((Cron.Elapsed - Offline_Period) * Time_Speedup, Changes);
                     --  Give 60 seconds lead start, accelerate x2 time elapsing
                  end if;

                  Continue := not Best_Sol.Completed;

                  if Changes and not Begun then
                     Log ("Mission execution started, first tasks frozen", Always);
                     Log ("Current assignation at mission start follows", Always);
                     Solver.Best_Solution.To_Assignment.Print_Assignment;
                     Draw.Draw_Assignment
                          (Solver.Best_Solution.To_Assignment);
                  end if;

                  if Changes then
                     Begun := True;
                     Solver.Set_Best_Solution (Best_Sol);
                     Solver.Set_Current_Solution (Best_Sol);
                  end if;
               end Progress;
            begin
               Solver.Solve (Solution,
                             Cooler'Access,
                             This.Iterations,
                             This.Annealing_Period,
                             Converge => This.Convergence_Period,
                             Progress => Progress'Access);
               Solution := Solver.Best_Solution;
            end;
         when Sancta.Totalsum =>
            declare
               Solver : Solver_Totalsum.Object;
            begin
               Solver.Solve (Solution,
                             Cooler'Access,
                             This.Iterations,
                             This.Annealing_Period,
                             Converge => This.Convergence_Period);
               Solution := Solver.Best_Solution;
            end;
         end case;

         Solution.To_Assignment.Print_Assignment;

         Log ("Planning ended, sending tasks", Trace.Debug, Section => Log_Section);
         --  Send_Plan_To_Agents (This, Mutable_Assignment.To_Assignment (Solution));
         Draw.Draw_Assignment (Solution.To_Assignment);
      end;
   end Replan;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration
     (This               : in out Object;
      Criterion          : in     Sancta.Assignment_Criteria := Sancta.Minimax;
      Iterations         : in     Positive                    := Default_Iterations;
      Annealing_Period   : in     Duration                    := Default_Annealing_Period;
      Convergence_Period : in     Duration                    := Default_Convergence_Period)
   is
   begin
      This.Criterion          := Criterion;
      This.Iterations         := Iterations;
      This.Annealing_Period   := Annealing_Period;
      This.Convergence_Period := Convergence_Period;
   end Set_Configuration;

end Sancta.Planner_Anneal;
