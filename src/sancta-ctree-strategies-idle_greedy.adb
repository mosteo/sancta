with Sancta.Ctree.Assigner;

with Sancta.Agent;
with Sancta.Agent.Handle;
with Sancta.Tasks.Insertions;
with Sancta.Tasks;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Ctree.Strategies.Idle_Greedy is

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class)
   is
      pragma Unreferenced (This);
   begin
      Ass := Perform (Ass.Get_Agents, Tasks, Costs, Links, Propag => True);
   end Perform;

   -------------
   -- Perform --
   -------------

   function Perform (Agents : Ac.Lists.List;
                     Tasks  : Tc.Lists.List;
                     Costs  : Sancta.Cost_Cache.Object'Class;
                     Links  : Connectivity_Matrix.Object'Class;
                     Propag : Boolean := True)
                     return   Sancta.Assignment.Object
   is
      Ass       : Sancta.Assignment.Object;
   begin
      Log ("AT IDLE_GREEDY" & Ass.Get_Idle_Agents.Length'Img,
           Debug, Log_Section);
      Ass.Set_Agents (Agents);
      while not Ass.Get_Idle_Agents.Is_Empty loop
         declare
            Agent     : constant Sancta.Agent.Object'Class :=
                          Ass.Get_Idle_Agents.First_Element;
            New_Agent : Sancta.Agent.Handle.Object;
            Tid       : Sancta.Tasks.Task_Id;
            Ct, Cd    : Sancta.Costs;
            use type Sancta.Tasks.Task_Id;
         begin
            Sancta.Tasks.Insertions.Greedy_Tail
              (Agent, Tasks, Costs,
               New_Agent, Tid, Ct, Cd);
            if Tid /= Sancta.Tasks.No_Task then
               Ass.Set_Agent (New_Agent.Get);
               if Propag then
                  Assigner.Copy_To_Linked (Ass, Agent.Get_Name, Links);
               end if;
            else
               return Ass; -- No assignable tasks?
            end if;
         end;
      end loop;
      pragma Assert (Ass.Get_Idle_Agents.Is_Empty, "Idle after greedy?");
      return Ass;
   end Perform;
end Sancta.Ctree.Strategies.Idle_Greedy;
