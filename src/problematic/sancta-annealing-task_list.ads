with Agpl.Optimization.Annealing.Solution;
with Sancta.Agent;
with Sancta.Agent.Handle;
with Sancta.Containers; use Sancta.Containers;

package Sancta.Annealing.Task_List is

--  Type and functions to do annealing with a single task list.
--  Not particularly light, I'm fucked up about optimizing anymore.

   Log_Section : constant String := "sancta.annealing.task_list";
   Det_Section : constant String := "sancta.annealing.task_list.detail";

   type Solution is new Agpl.Optimization.Annealing.Solution.Object
   with private;

   not overriding
   function Create (Bot   : Agent.Object'Class;
                    Tasks : Tc.Lists.List) return Solution;

   not overriding
   function Evaluate (This : Solution) return Cost;

   procedure Revert
     (This : in out Agpl.Optimization.Annealing.Solution.Object'Class);
   --  Revert to previous solution

   procedure Mutation_Swap
     (This : in out Agpl.Optimization.Annealing.Solution.Object'Class);
   --  Swap two random tasks

   not overriding
   function Get_Tasks (This : Solution) return Tc.Lists.List;

private

   type Solution is new Agpl.Optimization.Annealing.Solution.Object with record
      Bot   : Agent.Handle.Object;
      Tasks : Tc.Lists.List;
      Prev  : Tc.Lists.List; -- For undo
   end record;

end Sancta.Annealing.Task_List;
