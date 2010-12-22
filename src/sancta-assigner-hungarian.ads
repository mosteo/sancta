package Sancta.Assigner.Hungarian is

   pragma Preelaborate;

   type Object is new Assigner.Object with null record;

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return      Assignment.Object;

   --  This won't assign all tasks, but only one per agent at most.
   --  It is assumed that agents have no tasks; i.e. they're all idle.

   --  Utility function, for direct call:
   function Assign
     (Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return      Assignment.Object;

end Sancta.Assigner.Hungarian;
