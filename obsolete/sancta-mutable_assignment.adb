--  with Sancta.Debug2;
--  with Sancta.Draw;

with Agpl.Cr.Assigner;
with Agpl.Cr.Assigner.Hungry3;
with Agpl.Cr.Cost_Matrix;
with Agpl.Cr.Tasks.Starting_Pose;
with Agpl.Htn.Tasks.Handle;
with Agpl.Htn.Tasks.Utils;
with Agpl.Random;
with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

use Agpl;
use Agpl.Cr; -- This is too because the bug

with Ada.Numerics.Generic_Elementary_Functions;

package body Sancta.Mutable_Assignment is

   subtype Cost is Cr.Costs;
   use type Cost;

   package Acceptability_Math is new Ada.Numerics.Generic_Elementary_Functions
     (Optimization.Annealing.Acceptability);

   use Agpl.Optimization.Annealing;

   use type Htn.Plan_Node.Node_Access;

   ---------------
   -- Completed --
   ---------------

   function Completed (This : in Object) return Boolean is
   begin
      for I in This.Unfrozen.First_Index .. This.Unfrozen.Last_Index loop
         if This.Unfrozen.Element (I) <= Natural (This.Ass.Element (I).Length) then
            return False;
         end if;
      end loop;

      return True;
   end Completed;

   --------------------
   -- Evaluate_Agent --
   --------------------

   function Evaluate_Agent (This  : in Object;
                            Agent : in Positive -- Which agent
                           ) return   Agpl.Optimization.Cost
   is
      Agent_Proxy : constant Agpl.Cr.Agent.Object'Class :=
                      Agpl.Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (Agent));

      Tasks       : Subplan_Vectors.Vector renames This.Ass.Element (Agent);

      Total       : Cr.Costs;
   begin
      --  Initial task cost
      if Natural (Tasks.Length) > 0 then
         Total := Cost (Agent_Proxy.Get_Cost
                          (Htn.Plan_Node.Get_Task
                             (Tasks.Element (Tasks.First_Index))));

         for I in Tasks.First_Index .. Tasks.Last_Index - 1 loop
            Total := Total + Cost (Agent_Proxy.Get_Cost
                                     (Htn.Plan_Node.Get_Task (Tasks.Element (I)),
                                      Htn.Plan_Node.Get_Task (Tasks.Element (I + 1))));
         end loop;
         return Total;
      else
         return 0.0;
      end if;
   end Evaluate_Agent;

   ----------------------
   -- Evaluate_Minimax --
   ----------------------

   function Evaluate_Minimax
     (This : in Object) return Optimization.Cost
   is
      Worst : Cost := 0.0;
   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Worst := Cost'Max (Worst, Evaluate_Agent (This, I));
      end loop;

      return Worst;
   end Evaluate_Minimax;

   -----------------------
   -- Evaluate_Totalsum --
   -----------------------

   function Evaluate_Totalsum
     (This : in Object) return Optimization.Cost
   is
      Total : Cost := 0.0;
   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Total := Total + Evaluate_Agent (This, I);
      end loop;

      return Total;
   end Evaluate_Totalsum;

   ---------------------
   -- Flip_Given_Task --
   ---------------------

   procedure Flip_Given_Task (Pos : in Subplan_Vectors.Cursor) is
   begin
      if Subplan_Vectors.Has_Element (Pos) then
         Subplan_Vectors.Replace_Element (Pos,
                                          Htn.Plan_Node.Get_Random_Sibling
                                            (Subplan_Vectors.Element (Pos)));
      end if;
   end Flip_Given_Task;

   function Flip_Given_Task (Job : in Htn.Plan.Subplan) return Htn.Plan.Subplan
   is
      use Htn.Plan_Node;
   begin
      if Job /= null then
         return Get_Random_Sibling (Job);
      else
         return null;
      end if;
   end Flip_Given_Task;

   ----------------------
   -- Flip_Random_Task --
   ----------------------

   procedure Flip_Random_Task (This : in out Object) is
      Pos : Subplan_Vectors.Cursor;
      Job : Htn.Plan.Subplan;
   begin
      Select_Random_Task (This, Remove => False, Job => Job, Pos => Pos);
      Flip_Given_Task (Pos);
   end Flip_Random_Task;

   ---------------------
   -- Get_Agent_Tasks --
   ---------------------

   function Get_Agent_Tasks (This  : in Object;
                             Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List
   is
      Result : Htn.Tasks.Containers.Lists.List;
      use Htn.Tasks.Containers;
   begin
      for I in This.Ass.Element (Agent).First_Index ..
        This.Ass.Element (Agent).Last_Index
      loop
         Append (Result,
                 Htn.Plan_Node.Get_Task
                   (This.Ass.Element (Agent).Element (I)));
      end loop;

      return Result;
   end Get_Agent_Tasks;

   ------------------------------
   -- Get_Agent_Unfrozen_Tasks --
   ------------------------------

   function Get_Agent_Unfrozen_Tasks
     (This  : in Object;
      Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List
   is
      Result : Htn.Tasks.Containers.Lists.List;
      use Htn.Tasks.Containers;
   begin
      for I in This.Unfrozen.Element (Agent) ..
               This.Ass.Element (Agent).Last_Index
      loop
         Append (Result,
                 Htn.Plan_Node.Get_Task
                   (This.Ass.Element (Agent).Element (I)));
      end loop;

      return Result;
   end Get_Agent_Unfrozen_Tasks;

   ----------------------------------
   -- Get_Agents_With_Frozen_Tasks --
   ----------------------------------

   function Get_Agents_With_Frozen_Tasks (This : in Object)
                                          return    Agpl.Cr.Agent.Containers.Lists.List
   is
      Result : Cr.Agent.Containers.Lists.List;
   begin
      for I in This.Agent_Locator.First_Index .. This.Agent_Locator.Last_Index
      loop
         declare
            Agent : Cr.Agent.Object'Class :=
                      Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (I));
         begin
            Agent.Clear_Tasks;
            Agent.Set_Tasks (Get_Frozen_Tasks (This, I));
            Result.Append (Agent);
         end;
      end loop;
      return Result;
   end Get_Agents_With_Frozen_Tasks;

   ------------------------
   -- Get_Assigned_Tasks --
   ------------------------

   function Get_Assigned_Tasks (This : in Object) return Agpl.Htn.Tasks.Containers.Lists.List
   is
      Result : Htn.Tasks.Containers.Lists.List;
   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Htn.Tasks.Utils.Concatenate (Result,
                                            Get_Agent_Tasks (This, I));
      end loop;
      return Result;
   end Get_Assigned_Tasks;

   ---------------------------------
   -- Get_Assigned_Unfrozen_Tasks --
   ---------------------------------

   function Get_Assigned_Unfrozen_Tasks (This : in Object)
                                         return    Agpl.Htn.Tasks.Containers.Lists.List
   is
      Result : Htn.Tasks.Containers.Lists.List;
   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Htn.Tasks.Utils.Concatenate (Result,
                                            Get_Agent_Unfrozen_Tasks (This, I));
      end loop;
      return Result;
   end Get_Assigned_Unfrozen_Tasks;

   ----------------------
   -- Get_Frozen_Tasks --
   ----------------------

   function Get_Frozen_Tasks (This  : in Object;
                              Agent : in Positive) return Agpl.Htn.Tasks.Containers.Lists.List
   is
      Result : Htn.Tasks.Containers.Lists.List;
   begin
      pragma Assert (This.Ass.Element (Agent).Last_Index >=
                       This.Unfrozen.Element (Agent) - 1);

      for I in This.Ass.Element (Agent).First_Index ..
               This.Unfrozen.Element (Agent) - 1
      loop
         Htn.Tasks.Containers.Append
           (Result, Htn.Plan_Node.Get_Task (This.Ass.Element (Agent).Element (I)));
      end loop;
      return Result;
   end Get_Frozen_Tasks;

   ---------------------------
   -- Get_Most_Costly_Agent --
   ---------------------------

   function Get_Most_Costly_Agent (This : in Object) return Positive is
      Worst_Cost : Cost    := 0.0;
      Worst_Pos  : Natural := 0;
   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         declare
            Actual_Cost : constant Cost := Evaluate_Agent (This, I);
         begin
            if Worst_Cost < Actual_Cost then
               Worst_Cost := Actual_Cost;
               Worst_Pos  := I;
            end if;
         end;
      end loop;

      return Worst_Pos;
   end Get_Most_Costly_Agent;

   -------------------
   -- Heuristic_One --
   -------------------

   procedure Heuristic_One (This : in out Object; Which : in Natural := 0) is
      Target_Agent : Positive;
   begin
      if Which /= 0 then
         Target_Agent := Which;
      else
         Target_Agent := Random.Get_Integer
           (1, Positive (This.Ass.Length));
      end if;
      declare
         --  Choose an agent.
         Agent        : Cr.Agent.Object'Class renames
           Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (Target_Agent));

         Old_Plan     : Subplan_Vectors.Vector := This.Ass.Element (Target_Agent);
         New_Plan     : Subplan_Vectors.Vector;
         Last_Task    : Htn.Tasks.Handle.Object;
         --  Previously chosen task, for cost computation
      begin
         --        Log ("Initial cost: " &
         --             Strings.To_String (Float (Evaluate_Agent (This, Target_Agent))), Always);
         --        declare
         --           Ass : constant Cr.Assignment.Object := This.To_Assignment;
         --        begin
         --           Draw.Draw_Assignment (Ass);
         --        end;
         This.Agent_Ini := Target_Agent;
         Last_Task.Set (Agpl.Cr.Tasks.Starting_Pose.Create (Agent.Get_Name));

         --  Assing frozen tasks as is:
         for I in 1 .. This.Unfrozen.Element (Target_Agent) - 1 loop
            New_Plan.Append (Old_Plan.First_Element);
            Old_Plan.Delete_First;
         end loop;

         while not Old_Plan.Is_Empty loop
            declare
               Best_Subplan : Htn.Plan.Subplan;
               --  Task Chosen to be appended

               Best_Pos     : Subplan_Vectors.Cursor;
               --  Pointer to Old_Plan for position to be removed

               Best_Cost    : Cost := Cost'Last;
               --  Best insertion cost found for now

               -- Choose_Best_Task --
               procedure Choose_Best_Task is
                  use Htn.Plan_Node;
                  Curr_Cost    : Cost;
                  Curr_Subplan : Htn.Plan.Subplan;

                  procedure Choose_Best_Sibling (Node : in Htn.Plan.Subplan) is
                     --  PRE  : Get_Kind (Node) = Or_Node
                     Parent   : constant Htn.Plan.Subplan    := Get_Parent (Node);
                     Children : constant Node_Vectors.Vector := Get_Children (Parent);

                     Best_Sib : Htn.Plan.Subplan := Node;
                     Best_Cos : Cost := Cost (Agent.Get_Cost
                                                (Last_Task.Get, Get_Task (Node)));
                     --  We prefer current, if tie.
                  begin
                     --                    Log ("Examining" & Children.Length'Img & " siblings", Always);
                     for I in Children.First_Index .. Children.Last_Index loop
                        if Children.Element (I) /= Node then -- Already evaluate at initialization
                           if Get_Kind (Children.Element (I)) /= Task_Node or else
                             not Htn.Tasks.Is_Primitive (Get_Task (Children.Element (I)).all)
                           then
                              Log ("Mutable.Heuristic_One.Choose_Best_Sibling: Bailing out",
                                   Warning);
                              raise Program_Error;
                              --  COMPLETE BAIL OUT, NOT PREPARED FOR THIS
                           end if;

                           declare
                              Sib_Cost : constant Cost := Cost
                                (Agent.Get_Cost (Last_Task.Get,
                                                 Get_Task (Children.Element (I)).all));
                           begin
                              if Sib_Cost < Best_Cos then
                                 Best_Cos := Sib_Cost;
                                 Best_Sib := Children.Element (I);
                              end if;
                           end;
                        end if;
                     end loop;
                     Curr_Cost    := Best_Cos;
                     Curr_Subplan := Best_Sib;
                  end Choose_Best_Sibling;
               begin
                  for I in Old_Plan.First_Index .. Old_Plan.Last_Index loop
                     if Get_Parent (Old_Plan.Element (I)) = null or else
                       Get_Kind (Get_Parent (Old_Plan.Element (I))) /= Or_Node
                     then
                        Curr_Cost := Cost (Agent.Get_Cost
                                             (Last_Task.Get,
                                              Get_Task (Old_Plan.Element (I))));
                        Curr_Subplan := Old_Plan.Element (I);
                     else
                        Choose_Best_Sibling (Old_Plan.Element (I));
                     end if;

                     if Curr_Cost < Best_Cost then
                        Best_Cost    := Curr_Cost;
                        Best_Pos     := Old_Plan.To_Cursor (I);
                        Best_Subplan := Curr_Subplan;
                     end if;
                  end loop;
               end Choose_Best_Task;

            begin
               Choose_Best_Task;

               --              Log ("Selected cost: " & Strings.To_String (Float (Best_Cost)), Always);
               --              Log ("From task " & Last_Task.Get.To_String, Always);
               --              Log ("To task   " &
               --                   Htn.Tasks.To_String (Htn.Plan_Node.Get_Task (Best_Subplan)), Always);

               New_Plan.Append (Best_Subplan);
               Htn.Tasks.Handle.Set (Last_Task, Htn.Plan_Node.Get_Task (Best_Subplan));
               Old_Plan.Delete (Best_Pos);
            end;
         end loop;

         --  New assignation for the agent:
         This.Ass.Replace_Element (Target_Agent, New_Plan);

         --        Log ("Final cost: " &
         --             Strings.To_String (Float (Evaluate_Agent (This, Target_Agent))), Always);
         --
         --        declare
         --           Ass : constant Cr.Assignment.Object := This.To_Assignment;
         --        begin
         --           Draw.Draw_Assignment (Ass);
         --           Debug2.Wait_For_Keypress;
         --        end;
      end;
   end Heuristic_One;

   ----------------------
   -- Insert_At_Random --
   ----------------------

   procedure Insert_At_Random (This : in out Object;
                               Job  : in     Htn.Plan.Subplan)
   is
      Target_Agent : constant Positive := Random.Get_Integer
        (1, Positive (This.Ass.Length));
      --  Choose an agent.
      Target_Task  : constant Positive := Random.Get_Integer
        (This.Unfrozen.Element (Target_Agent),
         Natural (This.Ass.Element (Target_Agent).Length) + 1);
      --  Choose the place *before* the task we are to insert the new Job.
   begin
      pragma Assert (Target_Task >= This.Unfrozen.Element (Target_Agent));
      --  Maybe can fail when all agent tasks are frozen...

      This.Agent_Fin := Target_Agent;
      This.Task_Fin  := Target_Task;

      declare
         procedure Replace (Tasks : in out Subplan_Vectors.Vector) is
         begin
            Tasks.Insert (Before   => Target_Task,
                          New_Item => Job);
         end Replace;
      begin
         This.Ass.Update_Element (Target_Agent, Replace'Access);
      end;
   end Insert_At_Random;

   ------------------------
   -- Invalid_Assignment --
   ------------------------

   function Invalid_Assignment return Object is
      This : Object;
   begin
      This.Valid := False;
      return This;
   end Invalid_Assignment;

   -------------------
   -- Last_Mutation --
   -------------------

   function Last_Mutation (This : in Object) return String is
   begin
      return
        Mutations'Image (This.Last_Mutation) &
      This.Agent_Ini'Img & This.Task_Ini'Img & " " &
      This.Agent_Fin'Img & This.Task_Fin'Img & " " &
      Strings.To_String (Float (Evaluate_Minimax (This)));
   end Last_Mutation;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return This.Valid;
   end Is_Valid;

   ------------------
   -- Mark_Elapsed --
   ------------------

   procedure Mark_Elapsed (This    : in out Object;
                           Elapsed : in     Duration;
                           Changes :    out Boolean)
   is

      procedure Froze_Agent (I : in Positive) is
         use Optimization.Annealing;
         use Htn.Plan_Node;
         Acum_Cost : Cost := 0.0;
         Agent        : Cr.Agent.Object'Class renames
           Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (I));
         Tasks        : Subplan_Vectors.Vector renames This.Ass.Element (I);
         Last_Task    : Htn.Tasks.Handle.Object;
      begin
         Last_Task.Set (Agpl.Cr.Tasks.Starting_Pose.Create (Agent.Get_Name));
         for T in Tasks.First_Index .. Tasks.Last_Index loop
            declare
               New_Cost : constant Cost := Acum_Cost + Cost
                 (Agent.Get_Cost
                    (Last_Task.Get,
                     Get_Task (Tasks.Element (T))));
            begin
               if New_Cost > Cost (Elapsed) then
                  --  This is the first task to be over the edge, fronzen starts here.
                  Changes := Changes or This.Unfrozen.Element (I) /= T + 1;
                  This.Unfrozen.Replace_Element (I, T + 1);
--                    Log ("Agent" & I'Img & " unfrozen at" & Natural'Image (T+1),
--                         Always);
                  return;
               else
                  Acum_Cost := New_Cost;
               end if;
               Htn.Tasks.Handle.Set (Last_Task, Get_Task (Tasks.Element (T))); -- Arghhhhhh
            end;
         end loop;
         --  If we reach this point, it means no exceeding task has been found
         --  and all of them are frozen:
         Changes := Changes or This.Unfrozen.Element (I) /= Tasks.Last_Index + 1;
         This.Unfrozen.Replace_Element (I, Tasks.Last_Index + 1);
      end Froze_Agent;

   begin
      Changes := False;

      if Elapsed < 0.0 then
         return;
      end if;

      for I in This.Unfrozen.First_Index .. This.Unfrozen.Last_Index loop
         Froze_Agent (I);
      end loop;

--        if Changes then
--           Log ("Something froze at " & Strings.To_String (Float (Elapsed)), Always);
--           for I in This.Unfrozen.First_Index .. This.Unfrozen.Last_Index loop
--              Log ("Robot" & I'Img & " unfrozen at" & This.Unfrozen.Element (I)'Img, Always);
--           end loop;
--        end if;

   end Mark_Elapsed;

   ------------
   -- Mutate --
   ------------

   function Mutate (This : in Object) return Object is
      Result : Object := This;

      use Subplan_Vectors;
      use type Htn.Plan.Subplan;

      procedure Mutation_Heuristic_All is
         New_Assignment : constant Cr.Assignment.Object :=
                            Cr.Assigner.Hungry3.Assign
                              ((Cr.Assigner.Object with Keep_Order => True),
                               Get_Agents_With_Frozen_Tasks (This),
                               Get_Assigned_Unfrozen_Tasks (Result),
                               Cr.Cost_Matrix.Create_With_Start
                                 (Result.Context.Ref.Agents,
                                  Get_Assigned_Tasks (Result)));
      begin
         --  New_Assignment.Print_Assignment;
         Update (Result, New_Assignment);
      end Mutation_Heuristic_All;

      procedure Mutation_Flip is
         Pos : Subplan_Vectors.Cursor;
         Job : Htn.Plan.Subplan;
      begin
         if Random.Uniform <= 0.5 then
            --  Flip a random task
            Flip_Random_Task (Result);
            if Result.Task_Ini = 1 then
               Heuristic_One (Result, Result.Agent_Ini);
               Result.Last_Mutation := Flip_First_In_Place;
            else
               Result.Last_Mutation := Flip_In_Place;
            end if;
            Result.Valid := True;
         else
            --  Flip a task of the most costly agent
            Select_Random_Task_From_Agent
              (Result,
               Get_Most_Costly_Agent (Result),
               Remove => False,
               Job    => Job,
               Pos    => Pos);
            if Has_Element (Pos) then
               Flip_Given_Task (Pos);
               Result.Valid := True;
            end if;
            if Result.Task_Ini = 1 then
               Heuristic_One (Result, Result.Agent_Ini);
               Result.Last_Mutation := Guided_Flip_First_In_Place;
            else
               Result.Last_Mutation := Guided_Flip_In_Place;
            end if;
         end if;
      end Mutation_Flip;

      procedure Mutation_Move is
         Job : Htn.Plan.Subplan;
      begin
         Select_And_Remove_Random_Task (Result, Job);
         if Job /= null then
            Insert_At_Random (Result, Job);
            Result.Valid := True;
         end if;
      end Mutation_Move;

      procedure Mutation_Flip_Move is
         Job : Htn.Plan.Subplan;
      begin
         Select_And_Remove_Random_Task (Result, Job);

         if Job /= null then
            Job := Flip_Given_Task (Job);
            Insert_At_Random (Result, Job);
            Result.Valid := True;
         end if;
      end Mutation_Flip_Move;

      procedure Mutation_Guided_Move is
         Pos : Subplan_Vectors.Cursor;
         Job : Htn.Plan.Subplan;
      begin
         Select_Random_Task_From_Agent
           (Result,
            Get_Most_Costly_Agent (This),
            Remove => True,
            Pos => Pos,
            Job => Job);
         if Job /= null then
            Insert_At_Random (Result, Job);
            Result.Valid := True;
         end if;
      end Mutation_Guided_Move;

      procedure Mutation_Guided_Flip_Move is
         Pos : Subplan_Vectors.Cursor;
         Job : Htn.Plan.Subplan;
      begin
         Select_Random_Task_From_Agent
           (Result,
            Get_Most_Costly_Agent (This),
            Remove => True,
            Job    => Job,
            Pos    => Pos);
         if Job /= null then
            Job := Flip_Given_Task (Job);
            Insert_At_Random (Result, Job);
            Result.Valid := True;
         end if;
      end Mutation_Guided_Flip_Move;

      P : constant Float := Random.Uniform;
   begin
      Result.Agent_Ini := 0;
      Result.Agent_Fin := 0;
      Result.Task_Ini  := 0;
      Result.Task_Fin  := 0;

      if False then
         Heuristic_One (Result);
         Result.Last_Mutation := Heuristic_One;
      elsif P < 0.0004 then
         --  Full heuristic best assignation
         Mutation_Heuristic_All;
         Result.Last_Mutation := Heuristic_All;
      elsif P < 0.0005 then
         Heuristic_One (Result);
         Result.Last_Mutation := Heuristic_One;
      elsif P < 0.2 then
         --  Just flip a task
         Mutation_Flip;
      elsif P < 0.4 then
         --  Random moving of a task, without flip
         Mutation_Move;
         Result.Last_Mutation := Move;
      elsif P < 0.6 then
         --  Random moving + flip of a task
         Mutation_Flip_Move;
         Result.Last_Mutation := Flip_Move;
      elsif P < 0.8 then
         --  Random moving of a task from the most costly agent
         Mutation_Guided_Move;
         Result.Last_Mutation := Guided_Move;
      elsif P <= 1.0 then
         --  Random moving + flip of a task from the most costly agent
         Mutation_Guided_Flip_Move;
         Result.Last_Mutation := Guided_Flip_Move;
      end if;

      return Result;
   end Mutate;

   ---------------
   -- Normalize --
   ---------------

   function Normalize
     (Old_Cost,
      New_Cost : in Cost;
      Temp     : in Temperature)
      return        Acceptability
   is
      use Acceptability_Math;
   begin
      if New_Cost < Old_Cost then
         return Acceptability'Last;
      else
         return
           (Acceptability (Old_Cost / New_Cost) *
            Acceptability (Temp)) ** 0.75; -- We increase the wildlity a bit.
      end if;
   exception
      when Constraint_Error =>
         Log ("Old_Cost: " & Strings.To_String (Float (Old_Cost)), Error);
         Log ("New_Cost: " & Strings.To_String (Float (New_Cost)), Error);
         Log ("Temp    : " & Strings.To_String (Float (Temp)), Error);
         raise;
   end Normalize;

   -----------------------------------
   -- Select_And_Remove_Random_Task --
   -----------------------------------

   procedure Select_And_Remove_Random_Task
     (This : in out Object;
      Job  :    out Htn.Plan.Subplan)
   is
      Pos : Subplan_Vectors.Cursor;
   begin
      Select_Random_Task (This, Remove => True, Job => Job, Pos => Pos);
   end Select_And_Remove_Random_Task;

   ------------------------
   -- Select_Random_Task --
   ------------------------

   procedure Select_Random_Task (This   : in out Object;
                                 Remove : in     Boolean;
                                 Job    :    out Htn.Plan.Subplan;
                                 Pos    :    out Subplan_Vectors.Cursor)
   is
      Target_Agent : constant Positive := Random.Get_Integer
        (1, Positive (This.Ass.Length));
      --  Choose an agent.
   begin
      Select_Random_Task_From_Agent (This, Target_Agent, Remove, Job, Pos);
   end Select_Random_Task;

   -----------------------------------
   -- Select_Random_Task_From_Agent --
   -----------------------------------

   procedure Select_Random_Task_From_Agent (This   : in out Object;
                                            Agent  : in     Positive;
                                            Remove : in     Boolean;
                                            Job    :    out Htn.Plan.Subplan;
                                            Pos    :    out Subplan_Vectors.Cursor)
   is
      Target_Agent : Positive renames Agent;
      Target_Task  : Natural := Random.Get_Integer
        (This.Unfrozen.Element (Target_Agent), -- No selection of frozen tasks
         Natural (This.Ass.Element (Target_Agent).Length));
      --  Choose a task (can be 0 if no tasks in that agent).
   begin
      if Target_Task < This.Unfrozen.Element (Target_Agent) then
         Target_Task := 0;
      end if;

      This.Agent_Ini := Target_Agent;
      This.Task_Ini  := Target_Task;

      Job := null;

      if Target_Task = 0 then
         Pos := Subplan_Vectors.No_Element;
      else
         declare
            procedure Get_Ref (X : in out Subplan_Vectors.Vector) is
            begin
               if Remove then
                  Job := X.Element (Target_Task);
               else
                  Pos := X.To_Cursor (Target_Task);
               end if;
            end Get_Ref;
         begin
            This.Ass.Update_Element (Target_Agent, Get_Ref'Access);
         end;

         if Remove then
            declare
               procedure Delete (Tasks : in out Subplan_Vectors.Vector) is
               begin
                  Tasks.Delete (Target_Task);
               end Delete;
            begin
               This.Ass.Update_Element (Target_Agent, Delete'Access);
            end;
         end if;
      end if;
   end Select_Random_Task_From_Agent;

   ----------------
   -- To_Mutable --
   ----------------

   function To_Mutable (Agents : in Agpl.Cr.Agent.Containers.Lists.List;
                        Plan   : in Agpl.Htn.Plan.Object) return Object
   is
      Context : constant Context_Access := new Context_Type;
      Result  :          Object;
   begin
      Context.Agents := Agents;
      Context.Plan   := Htn.Plan.Inflate (Plan);

--      Htn.Plan.Print_Tree_Summary (Context.Plan);

      Result.Context.Bind (Context);

--      Log ("Preparing locator", Always);

      --  Prepare agent locator
      declare
         use Cr.Agent.Containers.Lists;
         I : Cursor := Result.Context.Ref.Agents.First;
      begin
         while Has_Element (I) loop
            Result.Agent_Locator.Append (I);
            Next (I);
         end loop;
      end;

--      Log ("Preparing empty assignations", Always);

      --  Prepare empty assignations and unfrozen tasks
      for I in Result.Agent_Locator.First_Index .. Result.Agent_Locator.Last_Index loop
         Result.Ass.Append (Subplan_Vectors.Empty_Vector);
         Result.Unfrozen.Append (1);
      end loop;

--      Log ("Done", Always);

      Result.Valid := False;

      return Result;
   end To_Mutable;

   -------------------
   -- To_Assignment --
   -------------------

   function To_Assignment (This : in Object)
                           return    Agpl.Cr.Assignment.Object
   is

      function Get_Agent (I : in Positive) return Cr.Agent.Object'Class is
         Ag : Cr.Agent.Object'Class :=
                Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (I));
      begin
         Ag.Set_Tasks (Get_Agent_Tasks (This, I));
         return Ag;
      end Get_Agent;

      Result : Cr.Assignment.Object;

   begin
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Result.Set_Agent (Get_Agent (I));
      end loop;

      return Result;
   end To_Assignment;

   ------------
   -- Update --
   ------------
   --  Reflexion... here we assume the task ids are proper in the plan
   --  We must update the assignation vectors to the new tasks in each agent.
   procedure Update (This       : in out Object;
                     Assignment : in     Agpl.Cr.Assignment.Object)
   is

      -- Update_Agent --
      procedure Update_Agent (I : in Positive) is
         Tasks : constant Htn.Tasks.Containers.Lists.List :=
                   Assignment.Get_Tasks
                     (Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (I)));
         use Htn.Tasks.Containers;
         use type Htn.Tasks.Task_Id;
         T         : Cursor := Tasks.First;
         New_Tasks : Subplan_Vectors.Vector;
      begin
         while Has_Element (T) loop
            New_Tasks.Append (Htn.Plan.Get_Node
                                (This.Context.Ref.Plan,
                                 Htn.Tasks.Get_Id (Element (T))));
            Next (T);
         end loop;

         --  Verify that frozen tasks are honored
         if False then
            for T in This.Ass.Element (I).First_Index ..
              This.Unfrozen.Element (I) - 1
            loop
               if This.Ass.Element (I).Element (T) /= New_Tasks.Element (T) then

                  This.To_Assignment.Print_Assignment;
                  Assignment.Print_Assignment;

                  for F in This.Unfrozen.First_Index .. This.Unfrozen.Last_Index loop
                     Log ("Unfrozen at" & This.Unfrozen.Element (F)'Img, Always);
                     pragma Assert
                       (This.Unfrozen.Element (F) - 1 =
                          Natural (Get_Frozen_Tasks (This, F).Length));
                  end loop;

                  raise Constraint_Error;
               end if;
            end loop;
         end if;

         This.Ass.Replace_Element (I, New_Tasks);
      end Update_Agent;

   begin
      if not Assignment.Is_Valid then
         This.Valid := False;
         return;
      end if;

--      Htn.Plan.Print_Tree_Summary (This.Context.Ref.Plan);

      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Update_Agent (I);
      end loop;

--        Print_Summary (This);
--
--        Log ("Updated mutable with cost " &
--             Strings.To_String (Float (Evaluate_Minimax (This))), Always);

      This.Valid := True;
   end Update;

   procedure Print_Summary (This : in Object) is

      procedure Print_Agent (I : in Positive) is
      begin
         Log ("** Agent :" &
              Cr.Agent.Containers.Lists.Element (This.Agent_Locator.Element (I)).Get_Name,
              Always);
         for J in This.Ass.Element (I).First_Index ..
                  This.Ass.Element (I).Last_Index
         loop
            Log (Htn.Tasks.Get_Id
                   (Htn.Plan_Node.Get_Task
                      (This.Ass.Element (I).Element (J)))'Img & "-" &
                 Htn.Tasks.To_String
                   (Htn.Plan_Node.Get_Task
                      (This.Ass.Element (I).Element (J)).all),
                 Always);
         end loop;
      end Print_Agent;

   begin
      Log ("** Mutable summary **", Always);
      for I in This.Ass.First_Index .. This.Ass.Last_Index loop
         Print_Agent (I);
      end loop;
      Log ("", Always);
   end Print_Summary;

end Sancta.Mutable_Assignment;
