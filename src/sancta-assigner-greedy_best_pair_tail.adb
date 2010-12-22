 

with Agpl.Chronos;
with Sancta.Agent.Handle;
with Sancta.Assignment;
with Sancta.Tasks.Insertions;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Handle;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Assigner.Greedy_Best_Pair_Tail is

   use type Agent.Containers.Lists.Cursor;
   use type Sancta.Tasks.Containers.Lists.Cursor;
   use type Sancta.Tasks.Task_Id;
   use type Sancta.Agent.Handle.Object;
   use type Sancta.Tasks.Handle.Object;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return Assignment.Object
   is
      A : Assignment.Object;
      --  The result we'll return.

      Pending : Sancta.Tasks.Containers.Lists.List := Tasks;
      --  Tasks not yet assigned.

      -------------------------
      -- Remove_From_Pending --
      -------------------------

      procedure Remove_From_Pending (Id : in Sancta.Tasks.Task_Id) is
         use Sancta.Tasks.Containers.Lists;
         I : Cursor := Pending.First;
      begin
         while Has_Element (I) loop
            if Element (I).Get_Id = Id then
               Pending.Delete (I);
               return;
            else
               Next (I);
            end if;
         end loop;
         raise Program_Error; -- Shouldn't be reached.
      end Remove_From_Pending;

      Timer : Agpl.Chronos.Object;
   begin
      --  Set agents
      declare
         use Sancta.Agent.Containers.Lists;
         procedure Add (I : Cursor) is
         begin
            A.Set_Agent (Element (I));
         end Add;
      begin
         Agents.Iterate (Add'Access);
      end;

      --  Assign tasks:
      while not Pending.Is_Empty loop
         Log ("Pending:" & Pending.Length'Img &
              " (Iter: " & Timer.Image & ")", Always);
         Timer.Reset;

         declare
            New_Ass : Sancta.Assignment.Object;
            New_Ag  : Agent.Handle.Object;
            use Sancta.Tasks;
            Id_Used : Task_Id;
            Cost_Total,
            Cost_Delta : Sancta.Costs;
         begin
            --  Insert best task in best agent:
            Sancta.Tasks.Insertions.Greedy_Tail (A,
                                             Pending,
                                             Costs,
                                             This.Criterion,
                                                 New_Ass,
                                                 New_Ag,
                                             Id_Used,
                                             Cost_Total,
                                             Cost_Delta);
            if Id_Used /= No_Task then
               A := New_Ass;
               Remove_From_Pending (Id_Used);
            else
               A.Set_Valid (False);
               return A;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Sancta.Assigner.Greedy_Best_Pair_Tail;
