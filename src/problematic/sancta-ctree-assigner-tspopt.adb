with Sancta.Agent.Utils;
with Sancta.Assigner.Mtsp_Concorde;
with Sancta.Tasks.Insertions;

package body Sancta.Ctree.Assigner.Tspopt is

   use type Sancta.Tasks.Task_Id;

   ----------------
   -- Build_Plan --
   ----------------

   procedure Build_Plan (This   : in out Object;
                         Agents :        Sancta.Agent.Containers.Lists.List;
                         Tasks  :        Sancta.Tasks.Containers.Lists.List;
                         Costs  :        Sancta.Cost_Cache.Object'Class;
                         Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
                         Plan   :    out Sancta.Tasks.Containers.Lists.List)
   is
      pragma Unreferenced (This, Links);
      Aux_Ass : Sancta.Assignment.Object;
      New_Ass : Sancta.Assignment.Object;
      Tid     : Sancta.Tasks.Task_Id;
   begin
      --  choose agent closer to any task:
      Aux_Ass.Set_Agents (Agents);
      Sancta.Tasks.Insertions.Greedy
        (Aux_Ass, Tasks, Costs, Criterion_Minmax, New_Ass, Tid);
      pragma Assert (Tid /= Sancta.Tasks.No_Task, "Failed to assign some task?");

      --  Assign in TSP fashion everything to this agent:
      declare
         Planner : Sancta.Assigner.Mtsp_Concorde.Object
           (Return_To_Base => False);
         Agent   : constant Sancta.Agent.Object'Class :=
                     New_Ass.Get_Agent (Tid).Get_Without_Tasks;
         Tspass  : constant Sancta.Assignment.Object  :=
                     Planner.Assign (Sancta.Agent.Utils.To_List (Agent),
                                     Tasks,
                                     Costs);
      begin
         Plan := Tspass.Get_Agent (Agent.Get_Name).Get_Tasks;
      end;
   end Build_Plan;

   -------------
   -- Process --
   -------------

--     procedure Process (This : in out Step_Proxy;
--                        Ctx  : in out Context.Object)
--     is
--     begin
--        This.Assigner.Set_Config (Ctx.Options);
--        This.Assigner.Assign (Ctx.Team.Get_Agents,
--                              Ctx.Pending_Tasks,
--                              Ctx.Pending_Tasks,
--                              Ctx.Costs,
--                              Ctx.Clusters,
--                              Ctx.Team,
--                              Ctx.Team);
--        Ctx.Plan := This.Assigner.Get_Plan;
--     end Process;

   -------------------------
   -- Register_Step_Proxy --
   -------------------------

--     procedure Register_Step_Proxy is
--     begin
--        Factory.Register (Steps'Image (Sancta.Ctree.Tspopt),
--                          new Step_Proxy);
--     end Register_Step_Proxy;

--  begin
--     Factory_Register (Sancta.Ctree.Tspopt,
--                       Object'(Preplanned.Object with null record));
end Sancta.Ctree.Assigner.Tspopt;
