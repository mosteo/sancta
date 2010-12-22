 

--  An assigner creates assignments. Ideally it should aim to achieve some kind
--  of optimality.

--  with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Tasks.Insertions;
with Sancta.Plan_Node;
with Sancta.Plan.Utils;
with Sancta.Tasks.Containers;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Plan_Assigner.Greedy1 is

   use type Sancta.Tasks.Task_Id;

   ------------
   -- Assign --
   ------------

   function Assign
     (This      : in Object;
      Agents    : in Agent.Containers.Vectors.Vector;
      Plan      : in Sancta.Plan.Object;
      Costs     : in Cost_Cache.Object'Class;
      Criterion : in Assignment_Criteria)
      return      Assignment.Object
   is
      pragma Unreferenced (This);
      A : Assignment.Object;
      P : Sancta.Plan.Object := Plan.Inflate; -- Fully expanded
   begin
--        Log ("T =" &
--             P.Enumerate_Tasks (Primitive => True, Pending   => True).Length'Img &
--             "; R =" & Agents.Length'Img, Always);

      --  Copy agents to the assignment.
      for I in Agents.First_Index .. Agents.Last_Index loop
         A.Set_Agent (Agents.Element (I));
      end loop;

      loop
         declare
            Pending : Sancta.Tasks.Containers.Lists.List :=
                        P.Enumerate_Tasks (Primitive => True,
                                           Pending   => True);
            New_Ass  : Assignment.Object;
            Inserted : Sancta.Tasks.Task_Id;
         begin
            --  Log ("Pending length is" & Pending.Length'Img, Always);
            exit when Pending.Is_Empty;

            Tasks.Insertions.Greedy (A,
                                     Pending,
                                     Costs,
                                     Criterion,
                                     New_Ass,
                                     Inserted);

            if Inserted /= Sancta.Tasks.No_Task then
               Log ("Succesfully assigned task" & Inserted'Img, Debug,
                    Section => Log_Section);
               A := New_Ass;

               Sancta.Plan.Utils.Trim_Or_Siblings (P, Inserted);
               Sancta.Plan_Node.Set_Finished (P.Get_Node (Inserted));

--                 P.Print_Tree_Summary;
--                 A.Print_Assignment;
--                 Log ("Cost: " & To_String
--                      (Float (A.Get_Cost (Costs, Criterion))), Always);
            else
               raise Constraint_Error
                 with "No feasible assignment found for all tasks: " &
                      "remaining =" & Pending.Length'Img;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Sancta.Plan_Assigner.Greedy1;
