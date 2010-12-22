--  Cache which has the same cost between any two tasks, as if robots were homogeneous.
--  Only the No_Id - Task pairs have a different cost per robot.

with Sancta.Agent.Containers;
with Sancta.Tasks.Containers;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Sancta.Cost_Cache.Homogeneous is

   pragma Preelaborate;

   --  type Object is interface;
   type Object is new Cost_Cache.Object with private;
   pragma Preelaborable_Initialization (Object);

   overriding
   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs);

   overriding
   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Costs;

   overriding
   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean;

   not overriding
   function Create (Agents : Agent.Containers.Lists.List;
                    Tasks  : Sancta.Tasks.Containers.Lists.List)
                    return   Object;
   --  Will create for No_Id tasks too.
   --  Also will call to add_initials

   not overriding
   procedure Add_Initials
     (This   : in out Object;
      Agents :        Agent.Containers.Lists.List;
      Tasks  :        Sancta.Tasks.Containers.Lists.List);
   --  Add costs for only initial tasks for each agent.

   Empty_Object : constant Object;

private

   package ATT_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Costs);

   use Att_Maps;

   type Object is new Cost_Cache.Object with record
      Cache : Att_Maps.Map;
   end record;

   Empty_Object : constant Object := (Cost_Cache.Object with others => <>);

end Sancta.Cost_Cache.Homogeneous;
