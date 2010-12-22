

--  A generic interface for cost caching strategies.

with Sancta.Agent,
     Sancta.Agent.Containers,
     Sancta.Tasks,
     Sancta.Tasks.Containers;

package Sancta.Cost_Cache is

   pragma Preelaborate;

   --  type Object is interface;
   type Object is abstract tagged null record;

   type Object_Access is access all Object'Class;

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs) is abstract;

   procedure Add_Costs
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class);
   --  Add all Agent.Get_Tasks -- Agent.Get_Tasks costs.
   --  Add all No_Task -- Agent.Get_Tasks costs

   procedure Add_Costs
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List);

   procedure Add_Costs
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class;
      Job   :        Tasks.Object'Class);
   --  Adds No_Task -- Job costs & Agent.Get_Tasks -- Job costs.
   --  With this one, costs can be added incrementally for newly appeared tasks


   procedure Add_Initials
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class);
   --  Add/Update the No_Task -- Agent.Get_Tasks costs.

   procedure Add_Initials
     (This   : in out Object;
      Agents :        Sancta.Agent.Containers.Lists.List);

   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean is abstract;

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Costs is abstract;

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in Sancta.Agent.Object'Class) return Costs;
   --  Say the full cost of an agent plan.

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in String;
      Tasks : in Sancta.Tasks.Containers.Lists.List) return Costs;
   --  Evaluate a plan with a given agent

   Empty_Object : constant Object'Class;

private

   type Empty_Class is new Object with null record;

   function Contains
     (This  : in Empty_Class;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean;

   function Get_Cost
     (This  : in Empty_Class;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Costs;

   procedure Set_Cost
     (This  : in out Empty_Class;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs) is null;

   Empty_Object : constant Object'Class := Empty_Class'(null record);

end Sancta.Cost_Cache;
