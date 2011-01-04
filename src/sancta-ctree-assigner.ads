with Sancta.Ctree.Connectivity_Matrix;

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Cost_Cache;
pragma Warnings (Off);
with Sancta.Criteria; use Sancta.Criteria;
pragma Warnings (On);
with Sancta.Tasks.Containers;

pragma Warnings (Off);
private with Agpl;
pragma Warnings (On);

package Sancta.Ctree.Assigner is

   pragma Elaborate_Body;

   Log_Section : constant String := "nerus.assigner";

   type Object is abstract tagged private;

   procedure Assign
     (This   : in out  Object;
      Agents :         Sancta.Agent.Containers.Lists.List;
      Assignable_Tasks : Sancta.Tasks.Containers.Lists.List;
      Live_Tasks       : Sancta.Tasks.Containers.Lists.List;
      Costs  :         Sancta.Cost_Cache.Object'Class;
      Links  :         Sancta.Ctree.Connectivity_Matrix.Object'Class;
      Old    :         Sancta.Assignment.Object;
      Ass    :    out  Sancta.Assignment.Object) is abstract;

   --  Assignable tasks are the ones to be assigned
   --  Live_Tasks are tasks that still are in the system, and are a superset of
   --  assignable tasks (some of the live tasks are already assigned and should not
   --  be assigned again).

   procedure Factory_Register (Strat : Alloc_Strategies;
                               This  : Object'Class);

   function  Factory_Create   (Strat : Alloc_Strategies) return Object'Class;

   procedure Remove_Linked (Ass   : in out Sancta.Assignment.Object'Class;
                            Bot   :        String;
                            Links :        Connectivity_Matrix.Object'Class);

   procedure Copy_To_Linked (Ass   : in out Sancta.Assignment.Object'Class;
                             Bot   : in     String;
                             Links :        Connectivity_Matrix.Object'Class);
   --  copy Bot tasks to linked robots

   procedure Copy_To_Linked (Ass   : in out Sancta.Assignment.Object'Class;
                             Bots  :        Sancta.Agent.Containers.Lists.List;
                             Links :        Connectivity_Matrix.Object'Class);
   --  If any of these robots has tasks, propagate.

private

   use Agpl;

   type Object is abstract tagged null record;

   package AC renames Sancta.Agent.Containers;
   package TC renames Sancta.Tasks.Containers;

end Sancta.Ctree.Assigner;
