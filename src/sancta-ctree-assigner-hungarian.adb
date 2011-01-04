with Sancta.Ctree.Strategies.Idle_Greedy;
with Sancta.Ctree.Weak_Grouping;

with Sancta.Agent.Extra;
with Sancta.Assigner.Hungarian;
--  with Agpl.If_Function;

package body Sancta.Ctree.Assigner.Hungarian is

--   function Iif is new Agpl.If_Function (Sancta.Tasks.Object'Class);

--     -------------
--     -- Predict --
--     -------------
--
--     --  Predict robot needs using the configured K, if requested.
--     --  For each detected task assigned with underestimation,
--     --   create a weak link between winner and closest robot.
--     --  Tasks are chosen by proximity to any robot
--
--     procedure Predict
--       (This   : in out Object;
--        Agents :        Sancta.Agent.Containers.Lists.List;
--        Assignable_Tasks : Sancta.Tasks.Containers.Lists.List;
--        Live_Tasks       : Sancta.Tasks.Containers.Lists.List;
--        Costs  :        Sancta.Cost_Cache.Object'Class;
--        Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
--        Old    :        Sancta.Assignment.Object;
--        Ass    : in out Sancta.Assignment.Object)
--     is
--        pragma Unreferenced (Assignable_Tasks, Agents, Live_Tasks, Old);
--
--        New_Links : Connectivity_Matrix.Object'Class := Links;
--        pragma Unreferenced (Links);
--
--        use Tc.Lists;
--        use Sancta.Tasks.Utils;
--
--        Unused : Sancta.Assignment.Object := Ass;
--        Used   : Sancta.Assignment.Object;
--
--        Pending : Tc.Lists.List := Unused.Get_All_First_Tasks;
--     begin
--        --  Log ("--> Predict", Always);
--        Copy_To_Linked
--          (Ass,
--           Ass.Get_Non_Idle_Agents,
--           New_links);
--
--        while not Pending.Is_Empty loop
--           declare
--              Groups    : constant Weak_Grouping.Object :=
--                            Weak_Grouping.Create (Unused, New_Links);
--
--              -------------------
--              -- Check_Enough --
--              -------------------
--
--              function Check_Enough (Job : Sancta.Tasks.Positioned.Object'Class)
--                                  return Boolean
--              is
--              begin
--                 return
--                   Groups.Get_Group_Size (Unused.Get_Agent (Job.Get_Id).Get_Name) >=
--                   Utils.Estimate_Bots_Needed (Job,
--                                               Sancta.Types.Origin,
--                                               This.Config);
--              end Check_Enough;
--
--              Job       : constant Sancta.Tasks.Object'Class :=
--                            Iif (This.Get_Config.Value (Config.Predict_With_Order),
--                                 Pending.First_Element,
--                                 Sancta.Tasks.Insertions.Closest_Task
--                                   (Unused.Get_Agents_Without_Tasks,
--                                    Pending,
--                                    Costs));
--
--           begin
--              exit when Groups.Num_Groups <= 1;
--
--              if Unused.Contains (Job.Get_Id) then
--                 if not Check_Enough
--                   (Sancta.Tasks.Positioned.Object'Class (Job))
--                 then
--                    --  Log ("Not enough", Always);
--                    --  Merge this task' owner with the closest in
--                    --  another group, if existing
--                    Groups.Merge_With_Closest
--                      (Unused.Get_Agent (Job.Get_Id).Get_Name,
--                       Unused,
--                       New_Links);
--                    Copy_To_Linked
--                      (Unused,
--                       Unused.Get_Agent (Job.Get_Id).Get_Name,
--                       New_Links);
--                 else
--  --                    Log ("ENOUGH" & Unused.Get_Agents (Job.Get_Id).Length'Img,
--  --                         Always);
--                    Used.Set_Agents
--                      (Groups.Get_Mates
--                         (Unused.Get_Agent
--                            (Job.Get_Id).Get_Name));
--                    Unused.Remove_Agents
--                      (Groups.Get_Mates
--                         (Unused.Get_Agent
--                            (Job.Get_Id).Get_Name));
--                    Pending := Pending - Job;
--                 end if;
--              else
--                 Pending := Pending - Job;
--              end if;
--           end;
--        end loop;
--        Ass.Set_Agents (Used.Get_Agents);
--        Ass.Set_Agents (Unused.Get_Agents);
--     end Predict;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (This   : in out Object;
      Agents :        Sancta.Agent.Containers.Lists.List;
      Assignable_Tasks : Sancta.Tasks.Containers.Lists.List;
      Live_Tasks       : Sancta.Tasks.Containers.Lists.List;
      Costs  :        Sancta.Cost_Cache.Object'Class;
      Links  :        Sancta.Ctree.Connectivity_Matrix.Object'Class;
      Old    :        Sancta.Assignment.Object;
      Ass    :    out Sancta.Assignment.Object)
   is
      pragma Unreferenced (This, Live_Tasks, Old);

      Tasks : Tc.Lists.List renames Assignable_Tasks;

      --  First assignment, hungarian
      --  One task/robot, no links taken into account
      First  : constant Sancta.Assignment.Object :=
        Sancta.Assigner.Hungarian.Assign (Agents, Tasks, Costs);

      --  Second, greedy fill
      --  Affects only excess robots without tasks.
      Second : constant Sancta.Assignment.Object :=
        Strategies.Idle_Greedy.Perform
          (First.Get_Agents, Tasks, Costs, Links, Propag => False);

      --  Keep only one leader
      --  This just creates the weak grouping of the current links
      Groups : constant Weak_Grouping.Object :=
        Weak_Grouping.Create (Second, Links);

      --  Reassign to leaders
      --  One task/robot, using only one cheap robot per group, and its task
      Third : constant Sancta.Assignment.Object :=
        Sancta.Assigner.Hungarian.Assign
          (Sancta.Agent.Extra.Remove_Tasks (Groups.Get_Cheapest_Leaders),
           Tasks, Costs);
   begin
--        Log ("--> Assign", Always);
--        Log ("# groups:" & Groups.Num_Groups'Img, Trace.Debug);
--         First.Print_Assignment;
--         Second.Print_Assignment;
--         Third.Print_Assignment;
      Ass := Third;

--        if This.Config.Value (Config.Predict_Superclusters) then
--           Ass.Merge_Missing_Robots (First, With_Tasks => False);
--  --           Log ("BEFORE", Always);
--  --           Ass.Print_Assignment;
--           Predict (This, Agents, Assignable_Tasks, Live_Tasks,
--                    Costs, Links, Old, Ass);
--  --           Log ("AFTER-", Always);
--  --           Ass.Print_Assignment;
--        end if;
   end Assign;

   ------------
   -- Assign --
   ------------

   function Assign
     (Agents : Sancta.Agent.containers.Lists.List;
      Tasks  : Sancta.Tasks.containers.Lists.List;
      Costs  : Sancta.Cost_Cache.Object'Class;
      Links  : Connectivity_Matrix.Object'Class)
      return Sancta.Assignment.Object
   is
      Dummy : Object;
      Ass   : Sancta.Assignment.Object;
   begin
      Dummy.Assign (Agents, Tasks, Tasks, Costs, Links, Ass, Ass);
      return Ass;
   end Assign;

--  begin
--     Factory_Register (Sancta.Ctree.Hungarian,
--                       Object'(Assigner.Object with null record));
end Sancta.Ctree.Assigner.Hungarian;
