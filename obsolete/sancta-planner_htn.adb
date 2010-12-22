with Sancta.Debug2;
--  with Sancta.Draw;

with Agpl.Chronos;
with Sancta.Plan_Node;
with Agpl.Trace; use Agpl.Trace;

with Ada.Text_Io;  use Ada.Text_Io;

package body Sancta.Planner_Htn is

   use type Sancta.Plan.Object;

   ------------------
   -- Obtain_Plans --
   ------------------

   function Obtain_Plans (This      : in Object;
                          Base_Plan : in Sancta.Plan.Object;
                          New_Tasks : in Sancta.Tasks.Containers.Lists.List)
                          return         Sancta.Plan.Object
   is
      pragma Unreferenced (This);

      Plan : Sancta.Plan.Object := Base_Plan; -- Modifiable working copy.
   begin
      --  Add pending tasks to plan:
      Sancta.Plan.Add_Subplan (Plan,
                            Sancta.Plan_Node.Create
                              (Sancta.Plan_Node.And_Node, New_Tasks));

      --  Obtain all expansions:
      declare
         Plans : Sancta.Plan.Object := Sancta.Plan.Expand (Plan);
      begin
         if Sancta.Plan.Is_Empty (Plans) then
            Log ("Planner_TSP: No valid plans found!", Informative);
         end if;

         return Plans;
      end;
   end Obtain_Plans;

   ------------
   -- Replan --
   ------------

   procedure Replan (This : in out Object) is
      New_Plans  : Sancta.Plan.Object;
      Best_Plan  : Sancta.Plan.Object;
      Assignment : Sancta.Assignment.Object;

      use Sancta.Plan_Node;
      use Sancta.Plan_Node.Node_Lists;
   begin

      Log ("Replan: Starting replanification", Debug, Section => Detail_Section);

      declare
         Cron : Agpl.Chronos.Object;
      begin
         New_Plans := Obtain_Plans (This,
                                    Get_Planner (This),
                                    Get_Pending_Tasks (This));
         Log ("Plan expansion duration: " & Cron.Image, Always);
      end;

      if New_Plans /= Sancta.Plan.Empty_Plan then

         Log (Length (Get_Children (Sancta.Plan.Get_Root (New_Plans)))'Img &
              " new plans obtained",
              Debug, Section => Detail_Section);

         --  Sancta.Plan.Print_Tree_Summary (New_Plans);

         declare
            Cron : Agpl.Chronos.Object;
         begin
            Select_Best_Plan (This,
                              Get_Alive_Agents (This),
                              New_Plans,
                              Best_Plan,
                              Assignment);
            Log ("Assignment selection duration: " & Cron.Image, Always);
         end;

         if Sancta.Plan.Is_Empty (Best_Plan) then
            Log ("Planner_HTN: No best plan found?", Warning, Section => Detail_Section);
         else
            Log ("Planner_HTN: Best plan identified", Debug, Section => Detail_Section);
           Log ("Plan cost is:" & Debug2.To_String (Assignment.Get_Cost (This.Criterion)),
                 Debug, Section => Detail_Section);
         end if;

         This.Current_Plan := Best_Plan;
         This.Assignment   := Assignment;

         --  Send tasks to corresponding agents!
         This.Send_Plan_To_Agents (Assignment);
      else
         Log ("Planification failed", Warning);
      end if;

      Sancta.Plan.Print_Tree_Summary (This.Current_Plan);
      Sancta.Assignment.Print_Assignment (This.Assignment);
   end Replan;

   ----------------------
   -- Select_Best_Plan --
   ----------------------

   procedure Select_Best_Plan (This       : in     Object;
                               Agents     : in     Sancta.Agent.Containers.Lists.List;
                               Plans      : in     Sancta.Plan.Object;
                               Best_Plan  :    out Sancta.Plan.Object;
                               Assignment :    out Sancta.Assignment.Object)
   is
      Assigner : constant Sancta.Assigner.Object'Class :=
                   This.Assigner.First_Element;

      procedure Draw_Plan (A : Sancta.Assignment.Object) is
      begin
         --  Draw.Draw_Assignment (A);
         raise Program_Error with "Unimplemented";
      exception
         when E : others =>
            Log ("Draw_Plan: " & Report (E), Error);
            raise;
      end Draw_Plan;

      Count : Positive := 1;
      procedure Count_Progress (A : Sancta.Assignment.Object) is
         pragma Unreferenced (A);
      begin
         Put (Count'Img);
         Count := Count + 1;
      end Count_Progress;

      procedure Dot_Progress (A : Sancta.Assignment.Object) is
         pragma Unreferenced (A);
      begin
         Put (".");
      end Dot_Progress;

   begin
      Sancta.Assigner.Assign_Best_Plan (Assigner,
                                    Agents,
                                    Plans,
                                    This.Criterion,
                                    Best_Plan,
                                    Assignment,
                                    Dot_Progress'Access);
      New_Line;

      --  Show best assignment
      Draw_Plan (Assignment);
   exception
      when E : others =>
         Log ("Planner_Sancta.Select_Best_Plan: " & Report (E), Warning);
         Log ("Planner_HTN: No best plan found", Warning);
         Best_Plan := Sancta.Plan.Empty_Plan;
   end Select_Best_Plan;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration (This      : in out Object;
                                Assigner  : in     Sancta.Assigner.Object'Class;
                                Criterion : in     Assignment_Criteria := Criterion_Minimax)
   is
   begin
      This.Assigner.Clear;
      This.Assigner.Append (Assigner);
      This.Criterion := Criterion;
   end Set_Configuration;

end Sancta.Planner_Htn;
