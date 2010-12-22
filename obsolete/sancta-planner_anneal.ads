 

--  A centralized planner using annealing.

with Sancta.Planner_Central;

with Sancta.Assignment;

--  This planner computes a solution using simulated annealing.
--  Every task received is expanded and added to a list of tasks
--  which are assigned with some greedy heuristic and then annealed.

package Sancta.Planner_Anneal is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "PlannerAnneal";
   Detail_Section : constant String := "PlannerAnneal.detail";
   --  To selectively enable debug messages...

   Default_Iterations         : constant Positive := Positive'Last;
   Default_Annealing_Period   : constant Duration := Duration'Last;
   Default_Convergence_Period : constant Duration := 10.0;

   type Object is new Planner_Central.Object with private;
   type Object_Access is access all Object'Class;

   procedure Replan (This : in out Object); -- Override
   --  Do all necessary replanning and task assignation and message sending and
   --  all that.

   procedure Set_Configuration
     (This               : in out Object;
      Criterion          : in     Sancta.Assignment_Criteria := Sancta.Minimax;
      Iterations         : in     Positive                    := Default_Iterations;
      Annealing_Period   : in     Duration                    := Default_Annealing_Period;
      Convergence_Period : in     Duration                    := Default_Convergence_Period);

private

   type Object is new Planner_Central.Object with
      record
         Convergence_Period : Duration := Default_Convergence_Period;
         Annealing_Period   : Duration := Default_Annealing_Period;
         Iterations         : Positive := Default_Iterations;

         Criterion          : Sancta.Assignment_Criteria := Sancta.Minimax;
         --  Assignment criterion

         Assignment         : Sancta.Assignment.Object;
         --  The current assignment for the plan.
      end record;

end Sancta.Planner_Anneal;
