with Sancta.Player.Draw;

package Sancta.Ctree.Assigner.Preplanned is

   --  This assigner receives an ordered list of tasks.
   --  These are then allocated in the given order using hungarian.
   --  Tasks are not propagated.
   --  There may be idle superclusters after assignation.

   pragma Elaborate_Body;

   Log_Section : constant String := "nerus.assigner.preplanned";

   type Object is new Assigner.Object with private;

   not overriding
   procedure Set_Plan (This : in out Object;
                       Plan :        Sancta.Tasks.containers.Lists.List);
   --  Use it to set a plan

   not overriding
   procedure Build_Plan (This   : in out Object;
                         Agents :        Sancta.Agent.Containers.Lists.List;
                         Tasks  :        Sancta.Tasks.Containers.Lists.List;
                         Costs  :        Sancta.Cost_Cache.Object'Class;
                         Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
                         Plan   :    out Sancta.Tasks.Containers.Lists.List)
   is null;
   --  This will be called when we have no plan and a replanning is requested.
   --  Derived classes may use it to build the plan here.

   overriding
   procedure Assign
     (This   : in out Object;
      Agents :        Sancta.Agent.Containers.Lists.List;
      Assignable_Tasks : Sancta.Tasks.Containers.Lists.List;
      Live_Tasks       : Sancta.Tasks.Containers.Lists.List;
      Costs  :        Sancta.Cost_Cache.Object'Class;
      Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
      Old    :        Sancta.Assignment.Object;
      Ass    :    out Sancta.Assignment.Object);
   --  This doesn't need to be overriden in descendents.

   not overriding
   function Get_Plan (This : Object) return Sancta.Tasks.Containers.Lists.List;

   not overriding
   procedure Draw (This   :        Object;
                   Ass    :        Sancta.Assignment.Object;
                   Drawer : in out Sancta.Player.Draw.Pg.Object) is null;
   --  May be overriding for ad-hoc drawing

private

   type Object is new Assigner.Object with record
      Plan       : Tc.Lists.List; -- Always the full plan, including finished tasks
   end record;

end Sancta.Ctree.Assigner.Preplanned;
