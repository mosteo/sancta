with Sancta.Ctree.Assigner.Preplanned;
--  with Sancta.Ctree.Context;
--  with Sancta.Ctree.Step;

package Sancta.Ctree.Assigner.Tspopt is

   --  Uses Concorde for optimally solving the assignation

   pragma Elaborate_Body;

   Log_Section : constant String := "nerus.assigner.tspopt";

   type Object is new Assigner.Preplanned.Object with null record;

   --  Assign is inherited.

   overriding
   procedure Build_Plan (This   : in out Object;
                         Agents :        Sancta.Agent.Containers.Lists.List;
                         Tasks  :        Sancta.Tasks.Containers.Lists.List;
                         Costs  :        Sancta.Cost_Cache.Object'Class;
                         Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
                         Plan   :    out Sancta.Tasks.Containers.Lists.List);

--     type Step_Proxy is new Step.Object with record
--        Assigner : Object;
--     end record;
--
--     overriding
--     procedure Process (This : in out Step_Proxy;
--                        Ctx  : in out Context.Object);
--
--     procedure Register_Step_Proxy;

end Sancta.Ctree.Assigner.Tspopt;
