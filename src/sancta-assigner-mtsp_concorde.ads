

with Sancta.Cost_Cache;

package Sancta.Assigner.MTSP_Concorde is

   Log_Section : constant String := "sancta.assigner.mtsp_concorde";

   type Object (Return_To_Base : Boolean) is
     new Assigner.Object with null record;

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
   --  Costs may contain Infinite costs.

   function Assign
     (Agents         : in Agent.Containers.Lists.List;
      Tasks          : in Sancta.Tasks.Containers.Lists.List;
      Costs          : in Sancta.Cost_Cache.Object'Class;
      Return_To_Base : in Boolean := False)
      return      Assignment.Object;
   --  For direct use without actual object

end Sancta.Assigner.MTSP_Concorde;
