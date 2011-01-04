with Sancta.Ctree.Assigner.Hungarian;
with Sancta.Ctree.Weak_Grouping;

with Sancta.Agent.Utils;
with Sancta.Tasks.Insertions;
use  Sancta;

with Agpl.Trace; use Agpl.Trace;

--  with Ada.Containers; use Ada.Containers;

package body Sancta.Ctree.Assigner.Preplanned is

   --------------
   -- Set_Plan --
   --------------

   procedure Set_Plan
     (This : in out Object;
      Plan :        Sancta.Tasks.containers.Lists.List)
   is
   begin
      This.Plan := Plan;
   end Set_Plan;

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
      pragma Unreferenced (Live_Tasks, Old);
      use Sancta.Tasks;

      Tasks : Tc.Lists.List renames Assignable_Tasks;

      ---------------------
      -- Purge_Completed --
      ---------------------

--        procedure Purge_Completed is
--           I : Tc.Lists.Cursor := This.Plan.First;
--           J : Tc.Lists.Cursor;
--        begin
--           --  At most, as many tasks as agents can have been completed.
--           for Max in 1 .. Agents.Length loop
--              exit when not Tc.Lists.Has_Element (I);
--              J := Tc.Lists.Next (I);
--              if not Sancta.Tasks.Insertions.Contains (Tasks,
--                                                   Tc.Lists.Element (I).get_id)
--              then
--                 This.Plan.Delete (I);
--              end if;
--              I := J;
--           end loop;
--        end Purge_Completed;

      -----------------
      -- Assign_Head --
      -----------------
      --  Returns unused tasks
      function Assign_Head return Tc.Lists.List is
         Groups   : constant Weak_Grouping.Object :=
           Weak_Grouping.Create (Ass, Links);
         Av_Tasks :          Tc.Lists.List;
         Hu_Ass   :          Sancta.Assignment.Object;
         Result   :          Tc.Lists.List := This.Plan;
      begin
         --  Assign as many tasks as groups exist:
         declare
            I : Tc.Lists.Cursor := This.Plan.First;
         begin
            while Natural (Av_Tasks.Length) < Groups.Num_Groups and then
                  Tc.Lists.Has_Element (I)
            loop
               --  We only use tasks in the plan that are also in the given list
               if Tasks.Contains (Tc.Lists.Element (I)) then
                  Av_Tasks.Append (Tc.Lists.Element (I));
               end if;
               Tc.Lists.Next (I);
            end loop;
         end;

         Log ("Sending" & Av_Tasks.Length'Img, Debug, Log_Section);

--         if This.Get_Config.Value (Config.Predict_Superclusters) then
         Log ("SCluster prediction is OFF", Debug, Log_Section);
--         end if;

         --  Here we must ensure that the assigned tasks are the first ones
         --  from the available ones. That is, no task is glossed over.
         --  This can only happen when cluster prediction is ON, since
         --  otherwise as many tasks as groups are always assigned.
         for Reassigning in 1 .. Natural'Last loop
            --  Assign tasks to bots
            Hu_Ass := Assigner.Hungarian.Assign
              (Agents, Av_Tasks, Costs, Links);

            exit when True; -- Fallout from S-Cluster prediction

            --  Avoid innecesary checks if not predicting.
--              exit when not This.Get_Config.Value (Config.Predict_Superclusters);
--
--              --  And now check.
--              declare
--                 Must_Retry : Boolean := False;
--                 Skipped    : Natural := 0;
--                 I          : Tc.Lists.Cursor := Av_Tasks.First;
--              begin
--                 --  Check that no task has been skipped.
--                 while Tc.Lists.Has_Element (I) loop
--                    if Hu_Ass.Contains (Tc.Lists.Element (I).Get_Id) then
--                       if Skipped > 0 then
--                          Must_Retry := True;
--                       end if;
--                    else
--                       Skipped := Skipped + 1;
--                    end if;
--                    Tc.Lists.Next (I);
--                 end loop;
--
--                 exit when not Must_Retry;
--
--                 Log ("Reassigning needed (" & Skipped'Img &
--                      " skipped)," & Reassigning'Img, Always);
--
--                 --  If so, remove as many as skipped and retry again.
--                 for I in 1 .. Skipped loop
--                    exit when Av_Tasks.Is_Empty;
--                    Av_Tasks.Delete_Last;
--                 end loop;
--              end;
         end loop;

         Ass.Set_Agents (Hu_Ass.Get_Agents);

         --  return unnasigned
         Sancta.Tasks.Insertions.Remove (Result, Hu_Ass.Get_All_Tasks, False);

         return Result;
      end Assign_Head;

      ---------------------------------
      -- Append_Remaining_To_Leaders --
      ---------------------------------

--        procedure Append_Remaining_To_Leaders (Av_Tasks : Tc.Lists.List) is
--           Ags : constant Ac.Lists.List := Ass.Get_Agents;
--           procedure Append (I : Ac.Lists.Cursor) is
--              Ag : Sancta.Agent.Object'Class := Ac.Lists.Element (I);
--           begin
--              if Ag.Has_Tasks then
--                 Ag.Add_Tasks (Av_Tasks);
--              end if;
--              Ass.Set_Agent (Ag);
--           end Append;
--        begin
--           Ags.Iterate (Append'Access);
--        end Append_Remaining_To_Leaders;

   begin
      if Tasks.Is_Empty then
         Ass.Clear;
         return;
      elsif This.Plan.Is_Empty then
         Build_Plan (Object'Class (This),
                     Agents, Tasks, Costs, Links, This.Plan);
      else
         null;
--           pragma Assert (Tasks.Length <= This.Plan.Length);
--           Purge_Completed;
--           Obsolete checks! The plan is kept in its entirety.
      end if;

      Ass.Set_Agents (Agents);

      declare
         Remain : constant Tc.Lists.List := Assign_Head;
         pragma Unreferenced (Remain);
      begin
         null;
         --  Append_Remaining_To_Leaders (Remain);
         --  just for aesthetic purposes but informative
         --  NO LONGER USED because it causes conflicts with keeping tasks across
         --  replannings.
      end;
      --        Ass.Print_Assignment;

      --  Finally assign from assignable to groups empty, no matter repeating.
      declare
         Groups  : constant Weak_Grouping.Object :=
                     Weak_Grouping.Create (Ass, Links);
         Pending : Ac.Lists.List := Groups.Get_Idle_Groups.Get_Agents;
         Fill    : Assignment.Object;
      begin
         --  Fill pending agents, no matter repeating tasks
         while not Pending.Is_Empty loop
            declare
               use Agent.Utils;
               New_Ass : constant Assignment.Object :=
                           Hungarian.Assign (Pending,
                                             Assignable_Tasks,
                                             Costs,
                                             Links);
            begin
               Fill.Set_Agents (New_Ass.Get_Agents);
               Pending := Pending - New_Ass.Get_Agents;
            end;
         end loop;
         --  Assign to leaders in group the best task:
         Ass.Set_Agents
           (Weak_Grouping.Create (Fill, Links).Get_Cheapest_Leaders);
      end;
   end Assign;

   --------------
   -- Get_Plan --
   --------------

   function Get_Plan (This : Object) return Sancta.Tasks.Containers.Lists.List is
   begin
      return This.Plan;
   end Get_Plan;

end Sancta.Ctree.Assigner.Preplanned;
