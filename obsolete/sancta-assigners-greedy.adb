with Sancta.Robot;
with Sancta.Sections;
with Sancta.Types;
with Sancta.Types.Operations;
with Sancta.Tasks.Positioned;

with Sancta.Tasks;
with Agpl.Trace;
use  Agpl;

package body Sancta.Assigners.Greedy is

   package Task_Lists renames Sancta.Tasks.Containers;

   use type Types.Real;
   use Agpl.Cr;
   use Agpl;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Matrix  : in Sancta.Cost_Matrix.Object)
      return Sancta.Assignment.Object
   is
      pragma Unreferenced (Matrix);

      A : Assignment.Object;
      --  The result we'll return.

      Agts   : Agent.Containers.Lists.List := Agents;
      --  Modifiable copy. Used in all the algorithm in place of Agents.

      Pending : Task_Lists.List := Tasks;
      --  Tasks not yet assigned.

      ------------------
      -- Closer_Agent --
      ------------------

      function Closer_Agent
        (Assigned_To : in Agent.Containers.Lists.Cursor;
         T           : in Sancta.Tasks.Positioned.Object)
         return           Agent.Containers.Lists.Cursor
      is
         use Agent.Containers.Lists;
         Closer : Cursor     := Assigned_To;
         Dist   : Types.Real := Types.Real'Last;
         D      : Types.Real;
         I      : Cursor := First (Agts);
         use type Types.Real;
      begin
         while I /= No_Element loop
            if I /= Assigned_To then
               declare
                  R  : Robot.Object renames Robot.Object (Element (I));
               begin
                  D := Types.Operations.Distance (T.Pose, Robot.Get_Last_Pose (R));
                  if D < Dist then
                     Dist   := D;
                     Closer := I;
                  end if;
               end;
            end if;
            Next (I);
         end loop;
         pragma Assert (Closer /= No_Element);
         return Closer;
      end Closer_Agent;

      -----------------
      -- Less_Costly --
      -----------------
      --  Says best least costly task for a given agent.

      function Less_Costly (Ag : in Agent.Object'Class;
                            T  : in Task_Lists.List)
                            return Task_Lists.Cursor
      is
         use Task_Lists;
         Best : Cursor := No_Element;
         Cost : Costs := Costs'Last;
         C    : Costs;
         I    : Cursor := First (T);
      begin
         while I /= No_Element loop
            if Agent.Has_Tasks (Ag) then
               C := Agent.Get_Cost (Ag, Agent.Get_Last_Task (Ag), Element (I));
            else
               C := Agent.Get_Cost (Ag, Element (I));
            end if;
            if C < Cost then
               Cost := C;
               Best := I;
            end if;

            Next (I);
         end loop;

         pragma Assert (Best /= No_Element);
         return Best;
      end Less_Costly;

      ---------------
      -- Less_Used --
      ---------------
      --  Says agent with least acummulated cost.

      function Less_Used return Agent.Containers.Lists.Cursor is
         use Agent.Containers.Lists;
         I    : Cursor := First (Agts);
         Best : Cursor := No_Element;
         Used : Costs  := Costs'Last;
      begin
         while I /= No_Element loop
            if Agent.Get_Plan_Cost (Element (I)) < Used then -- strict less. First agent passed is preferred.
               Used := Agent.Get_Plan_Cost (Element (I));
               Best := I;
            end if;
            Next (I);
         end loop;
         pragma Assert (Best /= No_Element);
         return Best;
      end Less_Used;

      Best      : Agent.Containers.Lists.Cursor;
      Closer    : Agent.Containers.Lists.Cursor;
      Best_Task : Task_Lists.Cursor;

      function "+" (I : Agent.Containers.Lists.Cursor) return Agent.Object'Class
        renames Agent.Containers.Lists.Element;
   begin
      Trace.Log ("Beginning Sancta.Assigners.Greedy...",
                 Trace.Debug, Sections.Planning);
      while not Pending.Is_Empty loop
         --  Select the less used agent.
         Best      := Less_Used;
         Trace.Log ("Less used agent is " & Agent.Get_Name (+Best),
                    Trace.Debug, Sections.Planning);

         --  Assing the less costly task to said agent.
         Best_Task := Less_Costly (Agent.Containers.Lists.Element (Best), Pending);

         if Task_Lists.Element (Best_Task) in Sancta.Tasks.Positioned.Object'Class then
            --  Check if someone else is closer to the task:
            declare
               Pose : Types.Pose renames
                 Sancta.Tasks.Positioned.Object
                   (Task_Lists.Element (Best_Task)).Pose;
               use type Agent.Containers.Lists.Cursor;
               Dist : Types.Real;
            begin
               Closer := Closer_Agent
                 (Best,
                  Sancta.Tasks.Positioned.Object (Task_Lists.Element (Best_Task)));
               Dist   := Types.Operations.Distance
                   (Robot.Get_Last_Pose (Robot.Object (Agent.Containers.Lists.Element (Closer))),
                    Pose);
--                 Trace.Log ("Distance between closer and task is" &
--                            Debug.To_String (Dist));
               if Closer /= Best and then
                 Types.Operations.Distance
                   (Robot.Get_Last_Pose (Robot.Object (Agent.Containers.Lists.Element (Closer))),
                    Pose) <= This.Umbral
               then
                  Best := Closer;
--                    Trace.Log ("Reassigned task to agent " &
--                               Agent.Get_Name (Agent.Containers.Lists.Element (Best)));
               else
                  null;
--                    Trace.Log ("Unable to reassign task");
               end if;
            end;
         end if;

--           Trace.Log ("Task " & Sancta.Tasks.To_String (Task_Lists.Element (Best_Task)) &
--                      " assigned to " & Agent.Get_Name (Agent.Containers.Lists.Element (Best)));

         declare
            procedure Assign (This : in out Agent.Object'Class) is
            begin
               Agent.Add_Task
                 (This, Task_Lists.Element (Best_Task));
            end Assign;
         begin
            Assignment.Add  (A, Agent.Containers.Lists.Element (Best),
                             Task_Lists.Element (Best_Task));
            Agent.Containers.Lists.Update_Element (Best, Assign'Access);
         end;

         --  Remove assigned task.
         Task_Lists.Delete (Pending, Best_Task);
      end loop;

      if Natural (Agent.Containers.Lists.Length (Agts)) < 2 then
         return A;
      end if;

      --  Second round optimization: every agent will reorder its tasks greedily.
      declare
         As : Assignment.Object;
         use Agent.Containers.Lists;
         I  : Cursor := First (Agents);

         -------------------
         -- Reorder_Agent --
         -------------------

         procedure Reorder_Agent (Ag : in Agent.Object'Class;
                                  Ta : in Task_Lists.List)
         is
            Rob      : Agent.Object'Class := Ag;
            Pending  : Task_Lists.List    := Ta;
            Cheapest : Task_Lists.Cursor;
         begin
--              Trace.Log ("Reordering tasks for agent " & Agent.Get_Name (Rob));
            while not Task_Lists.Is_Empty (Pending) loop
               Cheapest := Less_Costly (Rob, Pending);

               --  Append to robot
               Agent.Add_Task (Rob, Task_Lists.Element (Cheapest));

               --  Assign
               Assignment.Add (As, Rob, Task_Lists.Element (Cheapest));

               --  Remove task
               Task_Lists.Delete (Pending, Cheapest);
            end loop;
         end Reorder_Agent;

      begin
         --  Reset assignments:
         Agts := Agents;

         while I /= No_Element loop
            Reorder_Agent (Element (I), Assignment.Get_Tasks (A, Element (I)));
            Next (I);
         end loop;

         return As;
      end;
   end Assign;

end Sancta.Assigners.Greedy;
