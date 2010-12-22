

with Sancta.Assignment;
with Sancta.Tasks;
--  with Agpl.Strings;
--  with Agpl.Trace; use Agpl.Trace;

with Agpl.Text_Io; use Agpl.Text_Io;

package body Sancta.Assigner.Greedy_Minsum_Best_Pair_Tail is

   package Task_Lists renames Sancta.Tasks.Containers.Lists;
   use type Agent.Containers.Lists.Cursor;
   use type Task_Lists.Cursor;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Task_Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return Assignment.Object
   is
      pragma Unreferenced (This, Costs);

      A : Assignment.Object;
      --  The result we'll return.

      Agts  : Agent.Containers.Lists.List := Agents;
      --  Modifiable copy.

      Pending : Task_Lists.List := Tasks;
      --  Tasks not yet assigned.

      -----------------
      -- Less_Costly --
      -----------------
      --  Says best least costly task for a given agent.
      --  Best can be No_Element if no proper task for the agent.

      procedure Less_Costly
        (Ag   : in     Agent.Object'Class;
         Best :    out Sancta.Tasks.Containers.Lists.Cursor;
         Cost :    out Sancta.Costs)
      is
         use Task_Lists;
         C    : Sancta.Costs;
         I    : Cursor := First (Pending);
      begin
         Cost := Sancta.Costs'Last;
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
      end Less_Costly;

      ----------------
      -- Best_Combo --
      ----------------

      procedure Best_Combo (Best_Agent : out Agent.Containers.Lists.Cursor;
                            Best_Task  : out Sancta.Tasks.Containers.Lists.Cursor)
      is
         use Agent.Containers.Lists;
         I         : Cursor   := First (Agts);
         Best_Cost : Sancta.Costs := Sancta.Costs'Last;
      begin
         while I /= No_Element loop
            declare
               Agent_Cost : Sancta.Costs;
               Agent_Task : Sancta.Tasks.Containers.Lists.Cursor;
            begin
               Less_Costly (Element (I), Agent_Task, Agent_Cost);
               if Agent_Task /= Task_Lists.No_Element and then
                  Agent_Cost < Best_Cost
               then
                  Best_Agent := I;
                  Best_Task  := Agent_Task;
                  Best_Cost  := Agent_Cost;
               end if;
            end;
            Next (I);
         end loop;

--           if Has_Element (Best_Agent) then
--              Log ("Best Agent: " & Element (Best_Agent).Get_Name, Always);
--              Log ("Best Task : " & Sancta.Tasks.Containers.Element (Best_Task).To_String, Always);
--              Log ("Best Cost :" & Strings.To_String (Float (Best_Cost)), Always);
--           end if;
      end Best_Combo;

      Best_Agent : Agent.Containers.Lists.Cursor;
      Best_Task  : Task_Lists.Cursor;
   begin
      while not Pending.Is_Empty loop
         Put_Line ("Pending:" & Pending.Length'Img);

         --  Select the agent with a less costly task:
         Best_Combo (Best_Agent, Best_Task);

         if Best_Agent = Agent.Containers.Lists.No_Element then
            return Sancta.Assignment.Object
              (Sancta.Assignment.Invalid_Assignment);
         end if;

         declare
            procedure Assign (This : in out Agent.Object'Class) is
            begin
               Agent.Add_Task
                 (This, Task_Lists.Element (Best_Task));
            end Assign;
         begin
            Assignment.Add  (A,
                             Agent.Containers.Lists.Element (Best_Agent),
                             Task_Lists.Element (Best_Task));
            Agent.Containers.Lists.Update_Element (Agts, Best_Agent, Assign'Access);
         end;

         --  Remove assigned task.
         Task_Lists.Delete (Pending, Best_Task);
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Sancta.Assigner.Greedy_Minsum_Best_Pair_Tail;
