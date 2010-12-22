 

--  This assigner will chose in every step the best pairing amongst all tasks
--  in all expansions.

package Sancta.Plan_Assigner.Greedy1 is

   --  pragma Preelaborate;

   Log_Section : constant String := "Sancta.plan_assigner.greedy1";

   type Object is new Plan_Assigner.Object with null record;

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Sancta.Plan.Object;
      Costs     : in Cost_Cache.Object'Class;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object;
   --  Costs should include the starting task.

end Sancta.Plan_Assigner.Greedy1;
