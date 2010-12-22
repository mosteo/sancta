 

--  A centralized planner based in HTNs.
--  Customizable to use any Assigner.

with Sancta.Planner_Central;

with Sancta.Agent.Containers;
with Sancta.Assigner;
with Sancta.Assigner.Lists;
with Sancta.Assignment;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Plan;
with Sancta.Tasks.Containers;

package Sancta.Planner_Htn is

--    pragma Elaborate_Body;

   Log_Section    : constant String := "PlannerHTN";
   Detail_Section : constant String := "PlannerSancta.detail";
   --  To selectively enable debug messages...

   type Object is new Planner_Central.Object with private;

   type Object_Access is access all Object'Class;

   procedure Set_Configuration (This      : in out Object;
                                Assigner  : in     Sancta.Assigner.Object'Class;
                                Criterion : in     Assignment_Criteria := Criterion_Minimax);
   --  Wait: time to wait for a new task before start planning.
   --  Aliveness: time without notice from a bot until declaring it dead.

private

   function Obtain_Plans (This      : in Object;
                          Base_Plan : in Sancta.Plan.Object;
                          New_Tasks : in Sancta.Tasks.Containers.Lists.List)
                          return         Sancta.Plan.Object;
   --  Compute all possible plans combining a plan and some new tasks
   --  Will return Empty_Plan if no valid plan is found.

   procedure Replan (This   : in out Object); -- Override
   --  Expansion and selection of the best plan, sending to agents.

   procedure Select_Best_Plan (This       : in     Object;
                               Agents     : in     Sancta.Agent.Containers.Lists.List;
                               Plans      : in     Sancta.Plan.Object;
                               Best_Plan  :    out Sancta.Plan.Object;
                               Assignment :    out Sancta.Assignment.Object);
   --  Plans is an OR of possible plans.

   type Object is new Planner_Central.Object with
      record
         Assigner        : Sancta.Assigner.Lists.List;
         --  It will hold a single Assigner.

         Assignment      : Sancta.Assignment.Object;
         --  Current assignment found.

         Criterion       : Assignment_Criteria := Criterion_Minimax;
         --  Assignment criterion

         Current_Plan    : Sancta.Plan.Object;
         --  The current plan being run
      end record;

end Sancta.Planner_Htn;
