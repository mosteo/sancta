 

with Sancta.Tasks.Containers;

package Sancta.Plan.Utils.Random is

   --  pragma Elaborate_Body;

   Log_Section : constant String := "Sancta.plan.utils";

   function Get_Any_Expansion (This : in Plan.Object;
                               Jobs : in Tasks.Containers.Lists.List)
                               return    Tasks.Containers.Lists.List;
   --  Given a list of tasks, and a plan with some methods for expansion,
   --  will return the tasks in some arbitrary expansion.
   --  Tasks are expanded one by one so no exponential problem can occur
   --  with OR expansions.
   --  May raise constraint error if some task fails to expand

   function Get_Any_Expansion (This : in Plan.Object) return Plan.Object;
   --  Given a plan with OR nodes, return a random plan expansion.
   --  This is truly random and memory efficient, so don't worry about the
   --  number of OR nodes.

end Sancta.Plan.Utils.Random;
