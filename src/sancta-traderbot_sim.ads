--  To simulate several traderbots interacting.

with Sancta;
with Sancta.Agent;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Tasks;

--  Basic simulator of traderbots. Doesn't have many options.

package Sancta.Traderbot_Sim is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.traderbot_sim";
   Detail_Section : constant String := "sancta.traderbot_sim.detail";
   --  To selectively enable debug messages...

   type Object is tagged private;

   procedure Reset (This : in out Object);
   --  Remove all tasks and bots.

   procedure Add_Agent (This  : in out Object;
                        Agent : in     Sancta.Agent.Object'Class);
   --  Agent must have name and pose.
   --  No auction will be performed, so add them before adding the tasks.

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class);
   --  Auction this task.

   type Outcomes is (No_Change, Change_No_Improve, Change_Improve);

   procedure Auction_Random_Task (This     : in out Object;
                                  Changes  :    out Outcomes);
   --  Select a random task and auction it
   --  If it changes owner or place, Changes will be true

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cost_Matrix.Object);
   --  Costs to be used. Agents won't be used for cost evaluation

   procedure Set_Criterion (This : in out Object;
                            Crit : in     Assignment_Criteria);

   function To_Assignment (This : in Object) return Assignment.Object;
   --  Say the total cost incurred at present.

private

   type Object is tagged record
      Criterion : Assignment_Criteria  := Criterion_Invalid;
      Value     : Costs                := Infinite;

      Ass       : Sancta.Assignment.Object;
      Costs     : Sancta.Cost_Matrix.Object;
   end record;

end Sancta.Traderbot_Sim;
