with Ada.Containers.Vectors;

with Sancta.Agent.Utils; use Sancta.Agent.Utils;
-- with Agpl.Containers.Naked_Vectors;
with Sancta.Tasks.Utils;
with Agpl.Random;
--  with Agpl.Text_Io; use Agpl.Text_Io;

package body Sancta.Tasks.Insertions is

   use type Sancta.Costs;

   package Agent_Lists renames Agent.Containers.Lists;
   package Agent_Vectors renames Agent.Containers.Vectors;
   package Task_Lists renames Sancta.Tasks.Containers.Lists;

   use type Sancta.Agent.Handle.Object;
   package Candidate_Vectors is
     new Ada.Containers.Vectors (Positive, Sancta.Agent.Handle.Object);

   use type Sancta.Tasks.Task_Id;

   ---------------
   -- Before_Id --
   ---------------

   procedure Before_Id (List    : in out Sancta.Tasks.Containers.Lists.List;
                        Job     : in     Sancta.Tasks.Object'Class;
                        Id      : in     Sancta.Tasks.Task_Id;
                        Is_Last : in     Boolean := False)
   is
      use Sancta.Tasks.Containers.Lists;
      use type Sancta.Tasks.Task_Id;
   begin
      if Is_Last then
         List.Append (Job);
      else
         declare
            I : Cursor := List.First;
         begin
            while Element (I).Get_Id /= Id loop
               Next (I);
            end loop;
            if Has_Element (I) then
               List.Insert (Before => I, New_Item => Job);
            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Before_Id;

   ------------
   -- Greedy --
   ------------

   procedure Greedy
     (A          : in     Agent.Object'Class;
      T          : in     Sancta.Tasks.Object'Class;
      Not_Before : in     Natural;
      New_Agent  :    out Agent.Handle.Object;
      Cost_Delta :    out Sancta.Costs;
      Cost_Total :    out Sancta.Costs;
      Success    :    out Boolean)
   is
      use Sancta.Tasks.Containers.Lists;
      New_Tasks : List := A.Get_Tasks;
   begin
      New_Agent.Set (A);

      if New_Tasks.Is_Empty or else Natural (New_Tasks.Length) <= Not_Before then
         Cost_Delta := A.Get_Cost (T);
         Cost_Total := Cost_Delta;
         Success    := True;
         declare
            Aux : Sancta.Agent.Object'Class := A;
         begin
            Aux.Add_Task (T);
            New_Agent.Set (Aux);
         end;
      else
         declare
            Best_Pos    : Cursor := No_Element;
            --  Points to the point of insertion (next task of best point).
            Best_Cost   : Costs  := Costs'Last; -- New cost due to new task.
            Prev        : Cursor := No_Element;
            Curr        : Cursor := First (New_Tasks);
            Orig_Cost   : constant Costs :=
                            A.Get_Plan_Cost; -- Original plan cost.
         begin
            --  Skip Not_Before tasks:
            declare
               Counter : Natural := Not_Before;
            begin
               while Counter > 0 loop
                  Counter := Counter - 1;
                  Prev    := Curr;
                  Next (Curr);
               end loop;
            end;

            while Curr /= No_Element or else Prev /= No_Element loop
               declare
                  Curr_Cost   : Costs := Orig_Cost;
                  New_Cost_1  : Costs;
                  New_Cost_2  : Costs := 0.0;
               begin
                  --  First task special case:
                  if Prev = No_Element then
                     Curr_Cost  := Curr_Cost - A.Get_Cost (Element (Curr));
                     New_Cost_1 := A.Get_Cost (T);
                     New_Cost_2 := A.Get_Cost (T, Element (Curr));
                  elsif Curr = No_Element then -- Last task special case
                     New_Cost_1 := A.Get_Cost (Element (Prev), T);
                  else
                     Curr_Cost  := Curr_Cost -
                       A.Get_Cost (Element (Prev), Element (Curr));
                     New_Cost_1 := A.Get_Cost (Element (Prev), T);
                     New_Cost_2 := A.Get_Cost (T, Element (Curr));
                  end if;

                  if New_Cost_1 /= Costs'Last and then New_Cost_2 /= Costs'Last then
                     Curr_Cost := Curr_Cost + New_Cost_1 + New_Cost_2;

                     if Curr_Cost < Best_Cost then
                        Best_Cost := Curr_Cost;
                        Best_Pos  := Curr;
                     end if;
                  end if;
               end;

               Prev := Curr;
               Next (Curr);
            end loop;

            --  Insert at the best place:
            if Best_Cost = Costs'Last then
               Success := False;
            else
               Success    := True;
               Cost_Delta := Best_Cost - Orig_Cost;
               Cost_Total := Best_Cost;

               if Best_Pos = No_Element then -- last position
                  declare
                     Aux : Sancta.Agent.Object'Class := A;
                  begin
                     Aux.Add_Task (T);
                     New_Agent.Set (Aux);
                  end;
               else
                  declare
                     Aux : Sancta.Agent.Object'Class := A;
                  begin
                     Sancta.Tasks.Containers.Lists.Insert (New_Tasks, Best_Pos, T);
                     Aux.Set_Tasks (New_Tasks);
                     New_Agent.Set (Aux);
                  end;
               end if;
            end if;
         end;
      end if;
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Sancta.Tasks.Object'Class;
                     C          : in     Cost_Cache.Object'Class;
                     Not_Before : in     Natural;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Sancta.Costs;
                     Cost_Total :    out Sancta.Costs;
                     Success    :    out Boolean)
   is
      use Sancta.Tasks;
      use Sancta.Tasks.Containers.Lists;
      New_Tasks : List            := A.Get_Tasks;
      Name      : constant String := A.Get_Name;
   begin
      New_Agent.Set (A);

      if New_Tasks.Is_Empty or else Natural (New_Tasks.Length) < Not_Before then
         Cost_Delta := C.Get_Cost (Name, Sancta.Tasks.No_Task, Get_Id (T));
         Cost_Total := Cost_Delta;
         Success    := True;
         declare
            Aux : Sancta.Agent.Object'Class := A;
         begin
            Aux.Add_Task (T);
            New_Agent.Set (Aux);
         end;
      else
         declare
            Best_Pos    : Cursor := No_Element;
            --  Points to the point of insertion (next task of best point).
            Best_Cost   : Costs  := Costs'Last; -- New cost due to new task.
            Prev        : Cursor := No_Element;
            Curr        : Cursor := First (New_Tasks);
            Orig_Cost   : constant Costs := Cost_Cache.Get_Plan_Cost (C, A);
            --  Original plan cost.
         begin
            --  Skip Not_Before tasks:
            declare
               Counter : Natural := Not_Before;
            begin
               while Counter > 0 loop
                  Counter := Counter - 1;
                  Prev    := Curr;
                  Next (Curr);
               end loop;
            end;

            while Curr /= No_Element or else Prev /= No_Element loop
               declare
                  Curr_Cost   : Costs := Orig_Cost;
                  New_Cost_1  : Costs;
                  New_Cost_2  : Costs := 0.0;
               begin
                  --  First task special case:
                  if Prev = No_Element then
                     Curr_Cost  := Curr_Cost -
                       C.Get_Cost (Name, Sancta.Tasks.No_Task,
                                             Get_Id (Element (Curr)));
                     New_Cost_1 := Cost_Cache.Get_Cost
                       (C, Name, Sancta.Tasks.No_Task, Get_Id (T));
                     New_Cost_2 := Cost_Cache.Get_Cost
                       (C, Name, Get_Id (T), Get_Id (Element (Curr)));
                  elsif Curr = No_Element then -- Last task special case
                     New_Cost_1 := Cost_Cache.Get_Cost
                       (C, Name, Get_Id (Element (Prev)), Get_Id (T));
                  else
                     Curr_Cost  := Curr_Cost -
                       Cost_Cache.Get_Cost
                         (C, Name,
                          Get_Id (Element (Prev)), Get_Id (Element (Curr)));
                     New_Cost_1 := Cost_Cache.Get_Cost
                       (C, Name, Get_Id (Element (Prev)), Get_Id (T));
                     New_Cost_2 := Cost_Cache.Get_Cost
                       (C, Name, Get_Id (T), Get_Id (Element (Curr)));
                  end if;

                  if New_Cost_1 /= Sancta.Infinite and then
                    New_Cost_2 /= Sancta.Infinite
                  then
                     Curr_Cost := Curr_Cost + New_Cost_1 + New_Cost_2;

                     if Curr_Cost < Best_Cost then
                        Best_Cost := Curr_Cost;
                        Best_Pos  := Curr;
                     end if;
                  end if;
               end;

               Prev := Curr;
               Next (Curr);
            end loop;

            --  Insert at the best place:
            if Best_Cost = Sancta.Infinite then
               Success := False;
            else
               Success    := True;
               Cost_Delta := Best_Cost - Orig_Cost;
               Cost_Total := Best_Cost;

               if Best_Pos = No_Element then -- last position
                  declare
                     Aux : Sancta.Agent.Object'Class := A;
                  begin
                     Aux.Add_Task (T);
                     New_Agent.Set (Aux);
                  end;
               else
                  declare
                     Aux       : Sancta.Agent.Object'Class := A;
                  begin
                     Sancta.Tasks.Containers.Lists.Insert (New_Tasks, Best_Pos, T);
                     Aux.Set_Tasks (New_Tasks);
                     New_Agent.Set (Aux);
                  end;
               end if;
            end if;
         end;
      end if;
   end Greedy;

   procedure Greedy (A          : in     Agent.Object'Class;
                     T          : in     Sancta.Tasks.Object'Class;
                     New_Agent  :    out Agent.Handle.Object;
                     Cost_Delta :    out Sancta.Costs;
                     Cost_Total :    out Sancta.Costs;
                     Success    :    out Boolean) is
   begin
      Greedy(A, T, 0, New_Agent, Cost_Delta, Cost_Total, Success);
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (Ass       : in     Assignment.Object;
                     T         : in     Sancta.Tasks.Object'Class;
                     Costs     : in     Cost_Cache.Object'Class;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Success   :    out Boolean;
                     Random    : in     Boolean := False)
   is
      Agents : constant Agent.Containers.Lists.List := Ass.Get_Agents;

      Candids    : Candidate_Vectors.Vector;

      Best_Cost  : Sancta.Costs := Sancta.Infinite;

      procedure Check_Agent (I : in Sancta.Agent.Containers.Lists.Cursor) is
         New_Agent : Sancta.Agent.Handle.Object;
         New_Total,
         New_Delta : Sancta.Costs;
         New_Cost  : Sancta.Costs;
         Success   : Boolean;
      begin
--           Put_Line ("Trying insertion of " & T.To_String & " at agent " &
--                     Agent.Containers.Lists.Element (I).Get_Name & " with tasks:");
--           Sancta.Tasks.Utils.Print (Agent.Containers.Lists.Element (I).Get_Tasks);

         Greedy (Ac.Lists.Element (I),
                 T,
                 Costs,
                 Ac.Lists.Element (I).Get_Not_Before,
                 New_Agent,
                 Cost_Delta => New_Delta,
                 Cost_Total => New_Total,
                 Success    => Success);

         if Success then
            New_Cost := Evaluate (Criterion,
                                  Minmax => New_Total,
                                  Minsum => New_Delta,
                                  Minavg => 0.0);

            if New_Cost < Sancta.Infinite and then New_Cost = Best_Cost then
               Candids.Append (New_Agent);
            elsif New_Cost < Best_Cost then
               Best_Cost := New_Cost;
               Candids.Clear;
               Candids.Append (New_Agent);
            end if;
         end if;
      end Check_Agent;
   begin
      New_Ass := Ass;

      Agent.Containers.Lists.Iterate (Agents, Check_Agent'Access);

      if not Candids.Is_Empty then
         Success := True;
         if Random then
            New_Ass.Set_Agent
              (Candids.Element
                 (Agpl.Random.Get_Integer
                    (Candids.First_Index, Candids.Last_Index)).Get);
         else
            New_Ass.Set_Agent (Candids.Element (Candids.First_Index).Get);
         end if;
      else
         Success := False;
      end if;
   end Greedy;

   ------------
   -- Greedy --
   ------------

   procedure Greedy (Ass       : in     Assignment.Object;
                     Tasks     : in     Sancta.Tasks.Containers.Lists.List;
                     Costs     : in     Cost_Cache.Object'Class;
                     Criterion : in     Assignment_Criteria;
                     New_Ass   :    out Assignment.Object;
                     Inserted  :    out Sancta.Tasks.Task_Id;
                     Random    : in     Boolean := False)
   is
      Pending : constant Sancta.Tasks.Containers.Vectors.Vector :=
                  Sancta.Tasks.Utils.To_Vector (Tasks);
      Best_Cost     : Sancta.Costs := Infinite;
      pragma Optimization_Opportunity
        ("The new cost could be known without recomputing in full for each",
         "tried assignment, if the Greedy we are using passed more info out.");
   begin
      Inserted := Sancta.Tasks.No_Task;

      for I in Pending.First_Index .. Pending.Last_Index loop
         declare
            Temp_Ass        : Assignment.Object;
            Partial_Success : Boolean;
            Temp_Cost       : Sancta.Costs;
         begin
            Greedy (Ass,
                    Pending.Element (I),
                    Costs,
                    Criterion,
                    Temp_Ass,
                    Partial_Success,
                    Random);
            if Partial_Success then
               Temp_Cost := Temp_Ass.Get_Cost (Costs, Criterion);
               if Temp_Cost < Best_Cost then
                  New_Ass   := Temp_Ass;
                  Best_Cost := Temp_Cost;
                  Inserted  := Pending.Element (I).Get_Id;
               end if;
            end if;
         end;
      end loop;
   end Greedy;

   ------------
   -- Greedy --
   ------------

   function Greedy (Agents    : Ac.Lists.List;
                    Tasks     : Tc.Lists.List;
                    Costs     : Cost_Cache.Object'Class;
                    Criterion : Assignment_Criteria := Criterion_Minmax;
                    Random    : Boolean             := False)
                    return      Sancta.Assignment.Object
   is
      Result : Assignment.Object;
      Tid    : Sancta.Tasks.Task_Id;
   begin
      Greedy (Assignment.Object (Assignment.Create (Agents)),
              Tasks, Costs, Criterion,
              Result, Tid,
              Random);
      return Result;
   end Greedy;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Agent      : in Sancta.Agent.Object'Class;
                          Tasks      : in Sancta.Tasks.Containers.Lists.List;
                          Costs      : in     Cost_Cache.Object'Class;
                          New_Agent  :    out Sancta.Agent.Handle.Object;
                          Inserted   :    out Sancta.Tasks.Task_Id;
                          Cost_Total :    out Sancta.Costs;
                          Cost_Delta :    out Sancta.Costs)
   is
      use Task_Lists;

      Prev_Task : Sancta.Tasks.Task_Id := Sancta.Tasks.No_Task;
      Best_Cost : Sancta.Costs := Infinite;

      procedure Check (I : in Cursor) is
         Cost : constant Sancta.Costs := Cost_Cache.Get_Cost (Costs,
                                                           Agent.Get_Name,
                                                           Prev_Task,
                                                           Element (I).Get_Id);
         Temp_Agent : Sancta.Agent.Object'Class := Agent;
      begin
         if Cost < Best_Cost then
            Best_Cost := Cost;
            Temp_Agent.Set_Tasks (Agent.Get_Tasks);
            Temp_Agent.Add_Task (Element (I));
            New_Agent.Set (Temp_Agent);
            Inserted   := Element (I).Get_Id;
            Cost_Total := Cost_Cache.Get_Plan_Cost (Costs, Temp_Agent);
            Cost_Delta := Cost;
         end if;
      end Check;
   begin
      Inserted := Sancta.Tasks.No_Task;

      if Natural (Agent.Get_Tasks.Length) > 0 then
         Prev_Task := Agent.Get_Tasks.Last_Element.Get_Id;
      end if;

      Tasks.Iterate (Check'Access);
   end Greedy_Tail;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Ass        : in Assignment.Object;
                          T          : in Sancta.Tasks.Object'Class;
                          Costs      : in Cost_Cache.Object'Class;
                          Criterion  : in     Assignment_Criteria;
                          New_Ass    :    out Assignment.Object;
                          New_Agent  :    out Sancta.Agent.Handle.Object;
                          Cost_Total :    out Sancta.Costs;
                          Cost_Delta :    out Sancta.Costs;
                          Success    :    out Boolean)
   is
      Agents  : Agent_Lists.List  := Ass.Get_Agents;

      Best_Cost  : Sancta.Costs           := Infinite;
      Best_Agent : Agent_Lists.Cursor := Agent_Lists.No_Element;

      procedure Check_Agent (I : in Agent_Lists.Cursor) is
         Agent : Sancta.Agent.Object'Class := Agent_Lists.Element (I);
         Tasks  : Task_Lists.List       := Agent.Get_Tasks;
         Tasks2 : Task_Lists.List       := Tasks;
         Agent_Total : Sancta.Costs;
         Agent_Delta : Sancta.Costs;
      begin
         Tasks2.Append (T);
         Agent_Total := Cost_Cache.Get_Plan_Cost (Costs,
                                                   Agent.Get_Name,
                                                   Tasks2);
         Agent_Delta := Agent_Total - Cost_Cache.Get_Plan_Cost (Costs,
                                                                 Agent.Get_Name,
                                                                 Tasks);
         if Evaluate (Criterion,
                      Minmax => Agent_Total,
                      Minsum => Agent_Delta,
                      Minavg => 0.0) < Best_Cost then
            Best_Cost  := Evaluate (Criterion,
                                    Minmax => Agent_Total,
                                    Minsum => Agent_Delta,
                                    Minavg => 0.0);
            Best_Agent := I;
            Cost_Total := Agent_Total;
            Cost_Delta := Agent_Delta;
         end if;
      end Check_Agent;

   begin
      New_Ass := Ass;
      Agents.Iterate (Check_Agent'Access);
      if Best_Cost < Infinite then
         declare
            use Agent_Lists;
            Ag : Sancta.Agent.Object'Class :=
                   Ass.Get_Agent (Element (Best_Agent).Get_Name);
         begin
            Ag.Add_Task (T);
            New_Ass.Set_Agent (Ag);
            New_Agent.Set (Ag);
         end;
         Success := True;
      else
         Success := False;
      end if;
   end Greedy_Tail;

   -----------------
   -- Greedy_Tail --
   -----------------

   procedure Greedy_Tail (Ass       : in     Assignment.Object;
                          Tasks     : in     Sancta.Tasks.Containers.Lists.List;
                          Costs     : in     Cost_Cache.Object'Class;
                          Criterion : in     Assignment_Criteria;
                          New_Ass   :    out Assignment.Object;
                          New_Agent :    out Sancta.Agent.Handle.Object;
                          Inserted  :    out Sancta.Tasks.Task_Id;
                          Cost_Total:    out Sancta.Costs;
                          Cost_Delta:    out Sancta.Costs)
   is
      Pending : constant Sancta.Tasks.Containers.Vectors.Vector :=
                  Sancta.Tasks.Utils.To_Vector (Tasks);
      Best_Cost     : Sancta.Costs := Infinite;
   begin
      Inserted := Sancta.Tasks.No_Task;

      for I in Pending.First_Index .. Pending.Last_Index loop
         declare
            Temp_Ass        : Assignment.Object;
            Temp_Agent      : Sancta.Agent.Handle.Object;
            Partial_Success : Boolean;
            Temp_Cost,
            Temp_Total,
            Temp_Delta      : Sancta.Costs;
         begin
            Greedy_Tail (Ass,
                         Pending.Element (I),
                         Costs,
                         Criterion,
                         Temp_Ass,
                         Temp_Agent,
                         Temp_Total,
                         Temp_Delta,
                         Partial_Success);
            if Partial_Success then
               Temp_Cost := Evaluate (Criterion,
                                      Minmax => Temp_Total,
                                      Minsum => Temp_Delta,
                                      Minavg => 0.0);
               if Temp_Cost < Best_Cost then
                  New_Ass    := Temp_Ass;
                  New_Agent  := Temp_Agent;
                  Best_Cost  := Temp_Cost;
                  Inserted   := Pending.Element (I).Get_Id;
                  Cost_Total := Temp_Total;
                  Cost_Delta := Temp_Delta;
               end if;
            end if;
         end;
      end loop;
   end Greedy_Tail;

   ---------------
   -- Idle_Tail --
   ---------------

   procedure Idle_Tail (Ass        : in     Assignment.Object;
                        Tasks      : in     Sancta.Tasks.Containers.Lists.List;
                        Costs      : in     Cost_Cache.Object'Class;
                        New_Ass    :    out Assignment.Object;
                        Inserted   :    out Sancta.Tasks.Task_Id;
                        Cost_Total :    out Sancta.Costs;
                        Cost_Delta :    out Sancta.Costs)
   is
      Agents : constant Agent_Vectors.Vector := To_Vector (Ass.Get_Agents);

      Best_Cost  : Sancta.Costs := Infinite;
      Best_Agent : Agent_Vectors.Cursor := Agent_Vectors.No_Element;

      procedure Check (I : Agent_Vectors.Cursor) is
         Agent_Cost : constant Sancta.Costs :=
                        Cost_Cache.Get_Plan_Cost (Costs,
                                                   Agent_Vectors.Element (I));
      begin
         if Agent_Cost < Best_Cost then
            Best_Cost  := Agent_Cost;
            Best_Agent := I;
         end if;
      end Check;

      Agent_Total,
      Agent_Delta : Sancta.Costs;
      New_Agent   : Sancta.Agent.Handle.Object;
      Agent_Task  : Sancta.Tasks.Task_Id;
   begin
      if Natural (Tasks.Length) = 0 then
         Inserted := Sancta.Tasks.No_Task;
         return;
      end if;

      Agents.Iterate (Check'Access);

      if Agent_Vectors.Has_Element (Best_Agent) then
         Greedy_Tail (Agent_Vectors.Element (Best_Agent),
                      Tasks,
                      Costs,
                      New_Agent,
                      Agent_Task,
                      Agent_Total,
                      Agent_Delta);
         if Agent_Task = Sancta.Tasks.No_Task then
            Inserted := Sancta.Tasks.No_Task;
            return;
         end if;
         New_Ass    := Ass;
         New_Ass.Set_Agent (New_Agent.Get);
         Inserted   := Agent_Task;
         Cost_Total := Agent_Total;
         Cost_Delta := Agent_Delta;
      else
         Inserted := Sancta.Tasks.No_Task;
      end if;
   end Idle_Tail;

   ------------
   -- Remove --
   ------------

   procedure Remove (T  : in out Sancta.Tasks.Containers.Lists.List;
                     Id :        Sancta.Tasks.Task_Id;
                     Fail_If_Missing : Boolean := True)
   is
      use Sancta.Tasks.Containers.Lists;
      I : Cursor := T.First;
   begin
      while Has_Element (I) loop
         if Element (I).Get_Id = Id then
            T.Delete (I);
            return;
         else
            Next (I);
         end if;
      end loop;
      if Fail_If_Missing then
         raise Constraint_Error with "Task not found";
      end if;
   end Remove;

   --------------
   -- Contains --
   --------------

   function Contains (T  : Sancta.Tasks.Containers.Lists.List;
                      Id : Sancta.Tasks.Task_Id)
                      return Boolean
   is
      use Sancta.Tasks.Containers.Lists;
      I : Cursor := T.First;
   begin
      while Has_Element (I) loop
         if Element (I).Get_Id = Id then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

   ------------
   -- Remove --
   ------------

   procedure Remove (T : in out Sancta.Tasks.Containers.Lists.List;
                     X :        Sancta.Tasks.Containers.Lists.List;
                     Fail_If_Missing : Boolean := True)
   is
      procedure Remove_One (I : Tc.Lists.Cursor) is
      begin
         Remove (T, Tc.Lists.Element (I).Get_Id, Fail_If_Missing);
      end Remove_One;
   begin
      X.Iterate (Remove_One'Access);
   end Remove;

   -------------------
   -- Closest_Agent --
   -------------------

   function Closest_Agent (Agents    : Ac.Lists.List;
                           Tasks     : Tc.Lists.List;
                           Costs     : Cost_Cache.Object'Class;
                           Random    : Boolean := False)
                           return Sancta.Agent.Object'Class
   is
      Ass : constant Sancta.Assignment.Object :=
              Greedy (Agents, Tasks, Costs, Random => Random);
   begin
      if Natural (Ass.Get_All_Tasks.Length) /= 1 then
         raise Constraint_Error;
      else
         return Ass.Get_Agents.First_Element;
      end if;
   end Closest_Agent;

   ------------------
   -- Closest_Task --
   ------------------

   function Closest_Task (Agents    : Ac.Lists.List;
                          Tasks     : Tc.Lists.List;
                          Costs     : Cost_Cache.Object'Class;
                          Random    : Boolean := False)
                          return Sancta.Tasks.Object'Class
   is
      Ass : constant Sancta.Assignment.Object :=
              Greedy (Agents, Tasks, Costs, Random => Random);
   begin
      if Natural (Ass.Get_All_Tasks.Length) /= 1 then
         raise Constraint_Error;
      else
         return Ass.Get_All_Tasks.First_Element;
      end if;
   end Closest_Task;

   ------------------
   -- Closest_Task --
   ------------------

   function Closest_Task (Agents    : Ac.Lists.List;
                          Tasks     : Tc.Lists.List;
                          Costs     : Cost_Cache.Object'Class;
                          Random    : Boolean := False)
                          return Sancta.Tasks.Task_Id
   is
      Ass : constant Sancta.Assignment.Object :=
              Greedy (Agents, Tasks, Costs, Random => Random);
   begin
      if Natural (Ass.Get_All_Tasks.Length) = 0 then
         return Sancta.Tasks.No_Task;
      elsif Natural (Ass.Get_All_Tasks.Length) = 1 then
         return Ass.Get_All_Tasks.First_Element.Get_Id;
      else
         raise Constraint_Error with "Too many tasks?";
      end if;
   end Closest_Task;

end Sancta.Tasks.Insertions;
