with Sancta.Agent_Proxy;
with Sancta.Config;
--  with Sancta.Draw;
with Sancta.Methods.Choose_Entry_Point;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Problems.City43;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Types;
use  Sancta;

with Sancta.Agent.Containers;
with Sancta.Cost_Matrix;
with Sancta.Mutable_Assignment;
with Sancta.Method;
with Sancta.Plan;
with Sancta.Plan_Node;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Annealing;
with Agpl.Optimization.Annealing.Solver;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Calendar; use Ada.Calendar;
with Ada.Text_Io;  use Ada.Text_Io;

procedure Sancta.Main.Anneal_0605 is

--   Show_Partial_Solutions : constant Boolean := True;

   use Sancta.Mutable_Assignment;

   use type Sancta.Costs;

   package Solvers is new
     Agpl.Optimization.Annealing.Solver
       (Sancta.Mutable_Assignment.Object,
        Evaluate_Minimax,
        Mutate,
        Sancta.Mutable_Assignment.Normalize,
        Last_Mutation,
        Undo);

   ------------
   -- Cooler --
   ------------

   function Cooler is
     new Agpl.Optimization.Annealing.Cyclic_Cooling
       (Start  => Clock,
        Period => 20.0,
        Power  => 5.0);

   Solver    : Solvers.Object;

   Curr_Cost : Sancta.Costs := Sancta.Infinite;

   --------------
   -- Progress --
   --------------

   procedure Progress (Continue : out Boolean) is
      use Optimization.Annealing;
   begin
      Continue := True;

      if Solver.Best_Cost < Curr_Cost then
         Curr_Cost := Solver.Best_Cost;
         Log ("Better solution found", Always);
--           if Show_Partial_Solutions then
--              Draw.Draw_Assignment
--                (Solver.Best_Solution.To_Assignment);
--           end if;
      end if;
   end Progress;

   Plan   : Sancta.Plan.Object;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks is
      V : constant Sancta.Tasks.Containers.Vector := Problems.City43.Get_Tasks;
      L :          Sancta.Tasks.Containers.Lists.List;
   begin
      --  Segments
      for I in V.First_Index .. 5 loop -- V.Last_Index loop
         L.Append (V.Element (I));
      end loop;

      --  Entry points
      declare
         use type Types.Real;
         Entries : array (1 .. 4) of Tasks.Choose_Entry_Point.Object;
      begin
         Entries (1) := Tasks.Choose_Entry_Point.Create ((1 => ( 21.0,  0.0,  3.1)));
         Entries (2) := Tasks.Choose_Entry_Point.Create ((1 => (-24.0, -6.0,  0.0)));
         Entries (3) := Tasks.Choose_Entry_Point.Create ((1 => (-24.0, -6.0,  0.0)));
         Entries (4) := Tasks.Choose_Entry_Point.Create ((1 => ( 21.0,-18.0,  2.0)));

         for I in Entries'Range loop
            L.Append (Entries (I));
         end loop;
      end;

      --  Add all tasks as an AND node:
      Sancta.Plan.Add_Subplan
        (Plan, Sancta.Plan_Node.Create (Sancta.Plan_Node.And_Node, L));
   end Add_Tasks;

   Sol    : Sancta.Mutable_Assignment.Object;

   use type Types.Real;
begin
   Config.Init;
   Agpl.Trace.Enable_Section (Sancta.Mutable_Assignment.Log_Section);
   Agpl.Trace.Enable_Section (Sancta.Mutable_Assignment.Detail_Section);

   Log ("STARTING RUN ****************************************", Always);

   --  Config plan
   Sancta.Plan.Add_Method
     (Plan, Methods.Explore_Segment_Expansion.Object'
        (Sancta.Method.Object with null record));
   Sancta.Plan.Add_Method
     (Plan, Methods.Choose_Entry_Point.Object'
        (Sancta.Method.Object with null record));

   Add_Tasks;
   Plan := Sancta.Plan.Inflate (Plan);

--     Sancta.Plan.Print_Summary (Plan);
--     Sancta.Plan.Print_Tree_Summary (Plan);

   --  Prepare agents
   declare
      Al : Sancta.Agent.Containers.Lists.List;
      Ag : Agent_Proxy.Object;
   begin
      Ag.Set_Pose (( 21.0,  0.0,  3.1));
      Ag.Set_Name ("Ari");
      Al.Append (Ag);
      Add_Agent (Sol, Agent_Id (Ag.Get_Name));

      Ag.Set_Pose ((-24.0, -6.0,  0.0));
      Ag.Set_Name ("Ben");
      Al.Append (Ag);
      Add_Agent (Sol, Agent_Id (Ag.Get_Name));

      Ag.Set_Pose ((-24.0, -6.0,  0.0));
      Ag.Set_Name ("Ced");
      Al.Append (Ag);
      Add_Agent (Sol, Agent_Id (Ag.Get_Name));

      Ag.Set_Pose (( 21.0, -18.0,  2.0));
      Ag.Set_Name ("Dan");
      Al.Append (Ag);
      Add_Agent (Sol, Agent_Id (Ag.Get_Name));

      Sancta.Mutable_Assignment.Set_Costs
        (Sol, Sancta.Cost_Matrix.Create_With_Start
           (Al,
            Sancta.Plan.Enumerate_Tasks (Plan,
                                      Primitive => True,
                                      Pending   => True)));
   end;

   Set_Tasks (Sol, Plan);
   Create_Some_Solution (Sol, Sancta.Minimax);

   Log ("Initial assignment:", Always);
   Sol.To_Assignment.Print_Assignment;

   Log ("Solving...", Always);

   Solver.Solve (Sol,
                 Cooler'Access,
                 Iterations => Positive'Last,
                 Timeout    => Duration'Last,
                 Converge   => 60.0 * 60.0,
                 Progress   => Progress'Access);
exception
   when E : others =>
      Put_Line ("Main]: " & Report (E));
end Sancta.Main.Anneal_0605;
