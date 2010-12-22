 

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Cost_Cache;
with Sancta.Criteria; use Sancta.Criteria;
with Sancta.Plan;

package Sancta.Plan_Assigner is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Sancta.Plan.Object;
      Costs     : in Cost_Cache.Object'Class;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object is abstract;
   --  Costs should include the starting task.

end Sancta.Plan_Assigner;
