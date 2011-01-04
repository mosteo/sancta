package Sancta.Ctree.Assigner.Hungarian is

   --  pragma Preelaborate;

   type Object is new Assigner.Object with null record;

   procedure Assign
     (This             : in out Object;
      Agents           :        Sancta.Agent.Containers.Lists.List;
      Assignable_Tasks : Sancta.Tasks.Containers.Lists.List;
      Live_Tasks       : Sancta.Tasks.Containers.Lists.List;
      Costs            :        Sancta.Cost_Cache.Object'Class;
      Links            :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
      Old              :        Sancta.Assignment.Object;
      Ass              :    out Sancta.Assignment.Object);

   --  will do a first full assignment, hungarian + greedy fill;
   --  then remove the worst agents but one
   --  in each linked group, and reassign with the remaining agents.

   --  Convenience function for direct quicker use
   function Assign (Agents : Sancta.Agent.containers.Lists.List;
                    Tasks  : Sancta.Tasks.containers.Lists.List;
                    Costs  : Sancta.Cost_Cache.Object'Class;
                    Links  : Connectivity_Matrix.Object'Class)
                    return Sancta.Assignment.Object;

end Sancta.Ctree.Assigner.Hungarian;
