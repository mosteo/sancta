

--  A cost caching strategy using low space via computation on demand.

with Sancta.Agent_Proxy;

with Sancta.Containers; use Sancta.Containers;
with Sancta.Cost_Cache;
with Sancta.Tasks;

package Sancta.Cost_Proxy is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.cost_proxy";

   type Object is new Sancta.Cost_Cache.Object with private;
   --  This objects computes costs on demand, but additionally costs from
   --  No_Task can have a historic aditional component.

   overriding
   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs);

   overriding
   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean;

   overriding
   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Sancta.Costs;
   --  See about historic costs and tasks to understand how this really works.

   not overriding
   procedure Set_Agent (This  : in out Object;
                        Agent : in     Agent_Proxy.Object'Class);

   not overriding
   procedure Exclude_Agent (This  : in out Object;
                            Agent : in     String);

   not overriding
   procedure Clear_All_History (This : in out Object);

   not overriding
   procedure Add_To_Agent_Historic_Cost (This  : in out Object;
                                         Agent : in     String;
                                         Cost  : in     Sancta.Costs);

   not overriding
   function Get_Agent_Historic_Cost (This  : in Object;
                                     Agent : in String)
                                     return     Sancta.Costs;

   not overriding
   procedure Set_Agent_Historic_Cost (This  : in out Object;
                                      Agent : in     String;
                                      Cost  : in     Sancta.Costs);

   not overriding
   procedure Clear_Agent_Historic_Task
     (This  : in out Object;
      Agent : in     String);
   --  See below

   not overriding
   procedure Set_Agent_Historic_Task
     (This  : in out Object;
      Agent : in     String;
      Job   : in     Sancta.Tasks.Object'Class);
   --  When this exists, costs for No_Task -> Task are computed as
   --  Historic_Cost + Cost (Historic_Task, Task).
   --  When no historic task exists, cost is computed as
   --  Historic_Cost + Cost (Task)

   not overriding
   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class);

   not overriding
   procedure Add_Tasks (This  : in out Object;
                        Tasks : in     Task_Lists.List);
   --  Will keep old ones.

   not overriding
   function Contains (This : in Object;
                      Job  : in Sancta.Tasks.Task_Id) return Boolean;

private

   type Object is new Sancta.Cost_Cache.Object with record
      Agents    : Agent_Maps.Map;
      --  The proxying agents

      Historics : String_Cost_Maps.Map;
      --  The acummulated agent costs.

      Tasks     : Task_Maps.Map;
      --  The plan with the tasks to be used

      Historic_Tasks : Task_Containers.String_Element_Maps.Map;
      --  The last task executed by an agent.
   end record;

end Sancta.Cost_Proxy;
