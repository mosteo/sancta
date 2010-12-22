

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Sancta.Agent;
with Sancta.Agent.Containers;
with Sancta.Cost_Cache;
with Sancta.Tasks;
with Sancta.Tasks.Containers;

package Sancta.Cost_Matrix is

   pragma Preelaborate;

   Log_Section : constant String := "sancta.cost_matrix";

   type Object is new Cost_Cache.Object with private;
   --  Here we store a mapping of Agent x Start Task x End Task --> Costs
   --  This structure will be later used by assigners to compute an assignation.

   --  By default, creators here use "open" problems:
   --  returning to No_Task has Zero cost.
   --  Use Create_Closed_Costs to force these costs

   type Object_Access is access all Object'Class;

   Empty_Object : constant Object;

   procedure Create
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);
   --  Create a matrix given a list of agents and tasks to perform.
   --  Note, any old costs not overwritten will remain...
   --  Costs from No_Task are also added
   --  O (|A||T||T|)

   procedure Create
     (This   : in out Object;
      Agent  : in Sancta.Agent.Object'Class;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);
   --  Note, any old costs not overwritten will remain...
   --  O (|T||T|)

   function Create_With_Start
     (Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List) return Object;
   --  For compatibility. Does same as Create

   procedure Create_With_Start
     (This   : in out Object;
      Agent  : in Sancta.Agent.Object'Class;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);
   --  For compatibility. Does same as Create

   procedure Create_With_Start
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);
   --  For compatibility. Does same as Create

   procedure Create_Only_Start
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);
   --  Only costs for initial tasks from each robot

   procedure Create_Closed_Costs
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);

   procedure Merge (Dst : in out Object; Src : in Object);
   --  Overwrite Dst costs that are also present in Src, add new ones in Src.

   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean;
   --  Say if the cost is in here.

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Costs;
   --  Returns infinite if Agent-task-task combination is not present
   --  If Ini = Fin = No_Task, this should be explicitely informed by the
   --  caller if this has to represent something (i.e past costs)
   --  If no cost is found and Fin = No_Task then 0.0 is returned
   --  Else infinite is returned

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in Sancta.Agent.Object'Class) return Costs;
   --  Say the full cost of an agent plan.

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in String;
      Tasks : in Sancta.Tasks.Containers.Lists.List) return Costs;
   --  Evaluate a plan with a given agent

   overriding
   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs);

   procedure Print (This : in Object);
   --  Debug dump

   procedure Print_Diffs (L, R : in Object);

private

   package ATT_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Costs, Ada.Strings.Hash, "=", Sancta."=");

   use Att_Maps;

   type Object is new Cost_Cache.Object with record
      Matrix : Map;
   end record;

   function Key (Agent : in String;
                 Ini   : in Sancta.Tasks.Task_Id;
                 Fin   : in Sancta.Tasks.Task_Id) return String;
   pragma Inline (Key);
   --  Construct a suitable key for indexing.

   Empty_Object : constant Object := (Cost_Cache.Object with others => <>);

end Sancta.Cost_Matrix;
