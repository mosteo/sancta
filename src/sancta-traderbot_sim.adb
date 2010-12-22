with Sancta.Agent.Containers;
with Sancta.Agent.Utils;
with Sancta.Tasks.Insertions;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Utils;
with Agpl.Random;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

package body Sancta.Traderbot_Sim is

   package Agent_Vectors renames Sancta.Agent.Containers.Vectors;
   package Insertions renames Tasks.Insertions;
   package Task_Vectors renames Sancta.Tasks.Containers.Vectors;

   use type Assignment.Object;

   -------------------------
   -- Auction_Random_Task --
   -------------------------

   procedure Auction_Random_Task (This     : in out Object;
                                  Changes  :    out Outcomes)
   is
      use Sancta.Agent.Utils;
      use Sancta.Tasks.Utils;
      Agents       : constant Agent_Vectors.Vector :=
                       To_Vector (This.Ass.Get_Agents);
      Chosen_Agent : constant Integer :=
                       Random.Get_Integer (1, Natural (Agents.Length));
      New_Agent    : Sancta.Agent.Object'Class := Agents.Element (Chosen_Agent);
      Tasks        : constant Task_Vectors.Vector :=
                       To_Vector (New_Agent.Get_Tasks);
      Chosen_Task  : constant Integer :=
                       Random.Get_Integer (1, Natural (Tasks.Length));
      New_Ass      : Sancta.Assignment.Object;
   begin
      Changes  := No_Change;

      --  Agent without tasks?
      if Chosen_Task < 1 then
         return;
      end if;

      New_Ass  := This.Ass;

      --  Remove task from agent
      New_Agent.Remove_Task (Tasks.Element (Chosen_Task).Get_Id);

      New_Ass.Set_Agent (New_Agent);

      Log ("[Auction]" & Chosen_Task'Img & Chosen_Agent'Img,
           Debug, Detail_Section);

      --  Auction the task:
      declare
         Ok          : Boolean;
         New_New_Ass : Assignment.Object;
      begin
         Insertions.Greedy (New_Ass,
                            Tasks.Element (Chosen_Task),
                            This.Costs,
                            This.Criterion,
                            New_New_Ass,
                            Ok);
         if not Ok then
            raise Constraint_Error
              with "Unable to auction task " &
                   Tasks.Element (Chosen_Task).To_String;
         else
            declare
               New_Value : constant Costs :=
                             New_New_Ass.Get_Cost (This.Criterion);
            begin
               if New_Value < This.Value then
                  Log ("Value improved by auction", Debug, Detail_Section);
                  This.Ass   := New_New_Ass;
                  This.Value := New_Value;
                  Changes    := Change_Improve;
               elsif New_Value = This.Value and then This.Ass /= New_New_Ass then
                  Log ("Value remains equal, allocation changed", Debug, Detail_Section);
                  This.Ass   := New_New_Ass;
                  Changes    := Change_No_Improve;
               elsif New_Value > This.Value then
                  Log ("Value worsened by auction!?", Warning, Detail_Section);
                  --  raise Constraint_Error;
               elsif New_Value = This.Value and then This.Ass = New_New_Ass then
                  Log ("No changes after auction", Debug, Detail_Section);
               else
                  raise Program_Error;
               end if;
            end;
         end if;
      end;

   end Auction_Random_Task;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Object) is
   begin
      This.Ass.Clear;
      This.Ass.Set_Valid (True);
   end Reset;

   ---------------
   -- Add_Agent --
   ---------------

   procedure Add_Agent
     (This  : in out Object;
      Agent : in     Sancta.Agent.Object'Class)
   is
   begin
      This.Ass.Set_Agent (Agent);
   end Add_Agent;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (This : in out Object;
      Job  : in     Sancta.Tasks.Object'Class)
   is
      Ok      : Boolean;
      New_Ass : Assignment.Object;
   begin
      Insertions.Greedy (This.Ass,
                         Job,
                         This.Costs,
                         This.Criterion,
                         New_Ass,
                         Ok);
      if Ok then
         This.Ass   := New_Ass;
         This.Value := This.Ass.Get_Cost (This.Criterion);
      else
         raise Constraint_Error with "Could not assign task " & Job.To_String;
      end if;
   end Add_Task;

   -------------------
   -- To_Assignment --
   -------------------

   function To_Assignment (This : in Object) return Assignment.Object is
   begin
      return This.Ass;
   end To_Assignment;

   ---------------
   -- Set_Costs --
   ---------------

   procedure Set_Costs (This  : in out Object;
                        Costs : in     Cost_Matrix.Object) is
   begin
      This.Costs := Costs;
   end Set_Costs;

   -------------------
   -- Set_Criterion --
   -------------------

   procedure Set_Criterion (This : in out Object;
                            Crit : in     Assignment_Criteria)
   is
   begin
      This.Criterion := Crit;
   end Set_Criterion;


end Sancta.Traderbot_Sim;
