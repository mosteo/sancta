 

with Sancta.Cost_Cache;

package Sancta.Assigner.MTSP_Concordefake is

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object;
   --  Using the concorde solver.
   --  Works only with homogeneous robots, since costs are provided by the
   --  first agent in the list.
   --  Optimization is always MinSum
   --  Tasks *Mustn't* contain starting tasks

end Sancta.Assigner.Mtsp_Concordefake;
