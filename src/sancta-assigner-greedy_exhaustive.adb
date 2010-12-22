

with Agpl.Chronos;
with Sancta.Agent.Handle;
with Sancta.Assignment;
with Sancta.Insertion;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Handle;
with Sancta.Tasks.Utils; use Sancta.Tasks.Utils;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

package body Sancta.Assigner.Greedy_Exhaustive is

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

      Timer : Chronos.Object;
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
         Log ("Pending:" & Pending.Length'Img & " (" & Timer.Image & ")",
              Informative);
         Timer.Reset;

         declare
            --  Insert best task in best agent:
            Bid : constant Insertion.Bids :=
                    Insertion.Auction (A, Pending, This.Criterion, Costs);
            use Sancta.Tasks;
         begin
            if Bid.Task_Id /= No_Task then
               A := Insertion.Apply (A, Bid);
               Delete (Pending, Bid.Task_Id);
            else
               A.Set_Valid (False);
               return A;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

   ------------
   -- Assign --
   ------------

   function Assign
     (Criterion : Assignment_Criteria;
      Randomize : Boolean;
      Agents    : Agent.Containers.Lists.List;
      Tasks     : Sancta.Tasks.Containers.Lists.List;
      Costs     : Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object
   is
      This : constant Object := (Assigner.Object with Randomize, Criterion);
   begin
      return This.Assign (Agents, Tasks, Costs);
   end Assign;

end Sancta.Assigner.Greedy_Exhaustive;
