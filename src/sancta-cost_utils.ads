--  with Sancta.Traderbot_Types;
with Sancta.Types;

with Sancta;
with Sancta.Agent.Containers,
     Sancta.Auctions;
with Sancta.Cost_Matrix;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Concorde;
with Agpl; use Agpl;

package Sancta.Cost_Utils is

   package Task_Lists renames Sancta.Tasks.Containers.Lists;

   --   pragma Elaborate_Body;

   type Cost_Generator is abstract tagged null record;

   not overriding
   function Get_Cost (This : Cost_Generator;
                      Ini  : Sancta.Tasks.Object'Class;
                      Fin  : Sancta.Tasks.Object'Class)
                      return Sancta.Costs is abstract;
   --  Ini is the task just completed.
   --  Fin is the next task to execute and whose cost to complete we want.

   not overriding
   procedure Set_Pose (This : in out Cost_Generator;
                       Pose :        Types.Pose) is abstract;

   function Get_Concorde_Cost_Matrix
     (Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Matrix.Object) return Optimization.Concorde.Cost_Matrix;
   --  Will properly populate a concorde cost_matrix with these agents/tasks.
   --  Pre: Tasks include the Sancta.Tasks.Starting_Pose necessary tasks.
   --  Pre: The starting tasks are the first ones in the task list, in agent order.
   --  WARNING: UNIMPLEMENTED.


   --  For the following functions, we require that the agents are of class
   --  Sancta.Agent_Proxy (to be able to get its pose).
   --  And the tasks all of type Positioned (same reason).
   --  So we can draw schemes of cost and position.

   function Get_Execution_Cost (From, To  : in Sancta.Tasks.Object'Class;
                                Curr_Pose : in Types.Pose;
                                Lin_Speed,
                                Ang_Speed : in Types.Real)
                                return         Sancta.Costs;
   --  Centralized function will all task cost evaluations for use from
   --  Agent_Proxy, Robot and whichever needs it.
   pragma Assumption ("Non-Holonomic robot");

   function Get_Optimal_Cost
     (Agents  : in Sancta.Agent.Containers.Lists.List;
      Tasks   : in Sancta.Tasks.Containers.Lists.List) return Sancta.Costs;
   --  Get the optimal cost, according to TSP solution.

   function Get_Trader_Cost
     (Agents        : in Sancta.Agent.Containers.Lists.List;
      Tasks         : in Sancta.Tasks.Containers.Lists.List;
      Task_Policy   : in Auctions.Auction_Task_Policies;
      Insert_Policy : in Auctions.Insertion_Policies;
      Criterion     : in Assignment_Criteria) return Sancta.Costs;
   --  Get the cost resulting of a given traderbot policy, assuming the robots
   --  are at his starting positions.

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Commited_Task : in     Sancta.Tasks.Task_Id;
                                   Tasks         : in     Task_Lists.List);
   --  Will replace all 0 --> X costs with 0 --> Commited --> X.
   --  The idea is that the commited task is going to be performed in any case.
   --  So removing it from the plannable tasks and using this cost instead is
   --  proper.

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Tasks         : in     Task_Lists.List);
   --  As the previous one, but not the agent is queried for the cost of its
   --  plan and identification of its last task.

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Historic_Cost : in     Sancta.Costs;
                                   Commited_Task : in     Sancta.Tasks.Task_Id;
                                   Tasks         : in     Task_Lists.List);
   --  As previous, but now each task has prepended the Historic Cost till
   --  Commited_task (which is, accordingly, the last one completed)

end Sancta.Cost_Utils;
