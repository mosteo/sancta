with Sancta.Agent_Proxy;
with Sancta.Config;
with Sancta.Cost_Proxy;
with Sancta.Datastore;
with Sancta.Distributed.Aliveness;
with Sancta.Distributed.Datastore;
with Sancta.Distributed.Types;
--  with Sancta.Distributed.Types.Numeric;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Network;
with Sancta.Component.Factory; pragma Elaborate_All (Sancta.Component.Factory);
with Sancta.Component.Shared_Database;

with Agpl.Chronos;
with Agpl.Containers.String_Sets;
with Agpl.Conversions; use Agpl.Conversions;
with Sancta;
with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Mutable_Assignment.Auctions;
with Sancta.Mutable_Assignment.Moves;
with Sancta.Mutable_Assignment.Or_Mutations;
with Sancta.Plan;
with Sancta.Tasks.Containers;
with Agpl.Optimization;
with Agpl.Optimization.Annealing; pragma Elaborate_All (Agpl.Optimization.Annealing);
with Agpl.Optimization.Annealing.Solver;
with Agpl.Trace; use Agpl.Trace;
use  Agpl;

with Ada.Containers;
with Ada.Text_Io;

package body Sancta.Component.Annealer is

   package Agent_Maps renames Sancta.Agent.Containers.Maps;
   package Task_Lists renames Sancta.Tasks.Containers.Lists;

   use type Cost_Proxy.Object;
   use type Sancta.Plan.Object;
   use type Types.Pose;
   use type Ada.Containers.Count_Type;
   use type Agent_Maps.Map;
   use type Sancta.Assignment.Object;
   use type Sancta.Assignment_Criteria;
   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;
   use type Agpl.Optimization.Cost;

   Logfile : constant String := "anneal.log";

   function S is new Fixed_To_Str (Sancta.Costs);
   function S is new To_Str (Optimization.Cost);
   function S is new To_Str (Optimization.Annealing.Temperature);

   package Man_Temp is new Optimization.Annealing.Manual_Cooling
     (Initial_Temperature => 1.0,
      Ceiling_Temperature => 1.0,
      Cool_Time           => 0.5,
      Settle_Time         => 6.0,
      Divisor             => 1.85);

   function "+" (C : in Optimization.Cost) return Sancta.Costs; pragma Inline ("+");
   function "+" (C : in Optimization.Cost) return Sancta.Costs is
   begin
      if C = Optimization.Infinite then
         return Sancta.Infinite;
      else
         return Sancta.Costs (C);
      end if;
   end "+";

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (Job : in     Sancta.Tasks.Object'Class;
                       Ok  :    out Boolean)
   is
      Component   : constant Sancta.Component.Shared_Database.Object_Access :=
                   Sancta.Component.Shared_Database.Object_Access
                     (Config.Get_Plugin
                        (Sancta.Component.Shared_Database.Plugin_Name));
      use type Sancta.Component.Shared_Database.Object_Access;
   begin
      Ok := False;
      if Component /= null then
         declare
            Database           : constant Distributed.Datastore.Object_Access :=
                                   Component.Get;
            use type Distributed.Datastore.Object_Access;

            procedure Do_It (Key   : in     Distributed.Object_Key;
                             Value : in out Distributed.Object_Data'Class;
                             Meta  : in     Distributed.Object_Metadata)
            is
               pragma Unreferenced (Key, Meta);
               Context : Distributed.Types.Danneal renames
                 Distributed.Types.Danneal (Value);
            begin
               if Natural
                 (Context.Plan.Enumerate_Tasks
                    (Primitive => True,
                     Pending   => True,
                     Finished  => True).Length) > 2000 then
                  Log ("Discarding task, too many in plan!",
                       Warning, Log_Section);
               else
                  Ok := True;
                  Context.Plan.Add_Task (Job);
                  Context.Plan := Context.Plan.Inflate;
                  Log
                    ("Plan Size is" & Context.Plan.Size_In_Bytes'Img &
                     " and contains" &
                     Context.Plan.Enumerate_Tasks (Primitive => True,
                                                   Pending   => True,
                                                   Finished  => True).Length'Img &
                     " tasks",
                     Debug, Log_Section);
                  --  Add primitive tasks to cost proxy:
                  Cost_Proxy.Object
                    (Context.Agent_Costs.Ref.all).Add_Tasks
                    (Context.Plan.Enumerate_Tasks (Primitive => True,
                                                   Pending   => True,
                                                   Finished  => True));
                  Context.Assignment.Set_Valid (False);
                  Context.Ass_Cost := Sancta.Infinite;
               end if;
            end Do_It;

            Success : Boolean;
         begin
            if Database /= null and then Database.Contains (Shared_Context_Key) then
               Database.Update (Shared_Context_Key,
                                Do_It'Access,
                                Success,
                                Tout => 2.0);
               if Success then
                  Log ("Shared new task", Trace.Debug, Log_Section);
               else
                  Log ("Failed to share new task", Warning, Log_Section);
                  raise Program_Error;
               end if;
            end if;
         end;
      end if;
   end Add_Task;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Bot := Datastore.Robot
        (Datastore.Object.Get (This.Key (Requires_Agent))).Ref;

      This.Criterion :=
        Sancta.Value (Get_Attribute (Config, "criterion", "1.0 0.00001"));

      This.Log :=
        Boolean'Value (Get_Attribute (Config, "log", This.Log'Img));

      if This.Log then
         declare
            use Ada.Text_Io;
            F : File_Type;
         begin
            Create (F, Name => Logfile, Mode => Out_File);
            Close (F);
         end;
      end if;

      This.Anneal.Set_Criterion (This.Criterion);

      This.Execute :=
        Boolean'Value (Get_Attribute (Config, "execute", "True"));

      --  Mutations
--        --        This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Heuristic_1'Access,
--        --                                  Sancta.Mutable_Assignment.Undo_From_Scratch'Access);
--        --        This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Heuristic_2'Access,
--        --                                  Sancta.Mutable_Assignment.Undo_From_Scratch'Access);
--
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Moves.Do_Move_Task'Access,
                                Sancta.Mutable_Assignment.Moves.Undo_Move_Task'Access,
                                1.0);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Or_Mutations.Do_Switch_Or_Node'Access,
                                Sancta.Mutable_Assignment.Or_Mutations.Undo_Switch'Access,
                                1.0);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Moves.Do_Move_Task_Changing_Owner'Access,
                                Sancta.Mutable_Assignment.Moves.Undo_Move_Task'Access,
                                0.5);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Moves.Do_Guided_Move_Task_Changing_Owner'Access,
                                Sancta.Mutable_Assignment.Moves.Undo_Move_Task'Access,
                                1.0);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Auctions.Do_Auction_Task'Access,
                                Sancta.Mutable_Assignment.Auctions.Undo_Move_Task'Access,
                                0.5); -- 3.0
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Auctions.Do_Guided_Auction_Task'Access,
                                Sancta.Mutable_Assignment.Auctions.Undo_Move_Task'Access,
                                0.5); -- 1.0
--  --        This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Exhaustive_Auction_Task'Access,
--  --                                  Sancta.Mutable_Assignment.Undo_Move_Task'Access,
--  --                                  1.0);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Moves.Do_Swap_Order'Access,
                                Sancta.Mutable_Assignment.Moves.Undo_Move_Task'Access,
                                0.5);
      This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Moves.Do_Swap_Tasks'Access,
                                Sancta.Mutable_Assignment.Moves.Undo_Move_Task'Access,
                                0.5);
--  --        This.Anneal.Add_Mutation (Sancta.Mutable_Assignment.Do_Agent_Reorder'Access,
--  --                                  Sancta.Mutable_Assignment.Undo_From_Scratch'Access,
--  --                                  0.001);

      This.Annealer_Active.Start;

      return Component.Object_Access (This);
   end Create;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      This.Annealer_Active.Shutdown;
   end Stop;

   -------------------
   -- Set_Criterion --
   -------------------

   procedure Set_Criterion (Criterion : in Sancta.Assignment_Criteria) is
      Db : constant Distributed.Datastore.Object_Access :=
             Shared_Database.Get;
      use type Distributed.Datastore.Object_Access;
   begin
      if Db /= null then
         declare
            procedure Do_It (Key   : in     Distributed.Object_Key;
                                      Value : in out Distributed.Object_Data'Class;
                                      Meta  : in     Distributed.Object_Metadata)
            is
               pragma Unreferenced (Key, Meta);
               Context : Distributed.Types.Danneal renames
                 Distributed.Types.Danneal (Value);
            begin
               Log ("Solution cost was" & Context.Ass_Cost'Img, Always, Log_Section);
               Context.Criterion := Criterion;
               if Context.Assignment.Is_Valid then
                  Context.Ass_Cost := Context.Assignment.Get_Cost (Criterion);
               else
                  Context.Ass_Cost  := Sancta.Infinite;
               end if;
               Log ("Solution cost is now" & Context.Ass_Cost'Img, Always, Log_Section);
               Log ("Anneal criterion is now (" &
                    To_String (Criterion.Minmax_Weight, 5) & ", " &
                    To_String (Criterion.Minsum_Weight, 5) & ")",
                    Always, Log_Section);
            end Do_It;

            Ok : Boolean;
         begin
            Db.Update (Shared_Context_Key, Do_It'Access, Ok);
            if not Ok then
               Log ("Unable to toggle anneal executor", Warning, Log_Section);
            end if;
         end;
      else
         Log ("No distributed datastore component found", Warning, Log_Section);
      end if;
   end Set_Criterion;

   ------------
   -- Toggle --
   ------------

   procedure Toggle is
      Db : constant Distributed.Datastore.Object_Access :=
             Shared_Database.Get;
      use type Distributed.Datastore.Object_Access;
   begin
      if Db /= null then
         declare
            procedure Do_It (Key   : in     Distributed.Object_Key;
                                      Value : in out Distributed.Object_Data'Class;
                                      Meta  : in     Distributed.Object_Metadata)
            is
               pragma Unreferenced (Key, Meta);
               Context : Distributed.Types.Danneal renames
                 Distributed.Types.Danneal (Value);
            begin
               Context.Proceed := not Context.Proceed;
               Log ("Anneal executor is now " & Context.Proceed'Img,
                    Always, Log_Section);
            end Do_It;

            Ok : Boolean;
         begin
            Db.Update (Shared_Context_Key, Do_It'Access, Ok);
            if not Ok then
               Log ("Unable to toggle anneal executor", Warning, Log_Section);
            end if;
         end;
      else
         Log ("No distributed datastore component found", Warning, Log_Section);
      end if;
   end Toggle;

   -----------------
   -- Active_Task --
   -----------------

   task body Annealer_Task is
      Done     : Boolean := False;
      Database : constant Distributed.Datastore.Object_Access :=
                   Component.Shared_Database.Object_Access
                     (Config.Get_Plugin
                        (Component.Shared_Database.Plugin_Name)).Get;

      Changes_Cron : Chronos.Object;
      Improv_Timer : Chronos.Object;

      Local_Agents : Sancta.Agent.Containers.Maps.Map;
      Local_Costs  : Cost_Proxy.Object;
      Local_Plan   : Sancta.Plan.Object;
      Local_Ass    : Sancta.Assignment.Object;
      Name         : String (1 .. 3);

      function Shared_Context return Distributed.Types.Danneal;
      procedure Update_Pose;

      type Bot_States is (Idle, Executing);

      Prev_Task     : Sancta.Tasks.Task_Id := Sancta.Tasks.No_Task;
      --  Last executed task
      Curr_Task     : Sancta.Tasks.Task_Id := Sancta.Tasks.No_Task;
      --  Currently executing task
      Bot_Status    : Bot_States := Idle;

      function Evaluate_Here (Sol : in Sancta.Mutable_Assignment.Object)
                              return   Optimization.Cost;

      package Solvers is new
        Optimization.Annealing.Solver
          (Sancta.Mutable_Assignment.Object,
           Evaluate_Here,
           Sancta.Mutable_Assignment.Mutate,
           Sancta.Mutable_Assignment.Normalize,
           Sancta.Mutable_Assignment.Last_Mutation,
           Sancta.Mutable_Assignment.Undo);

      Solver       : Solvers.Object;

      ------------------
      -- Assign_Tasks --
      ------------------
      Assign_Timer : Chronos.Object;
      procedure Assign_Tasks is
         use Distributed.Types;

         function Is_In (L : Task_Lists.List; Id : Sancta.Tasks.Task_Id)
                         return Boolean
         is
            use Task_Lists;
            I : Cursor := L.First;
         begin
            while Has_Element (I) loop
               if Element (I).Get_Id = Id then
                  Log ("Id " & Id'Img & " is here", Never);
                  return True;
               end if;
               Log ("Id " & Element (I).Get_Id'Img & " mismatch", Never);
               Next (I);
            end loop;
            return False;
         end Is_In;
      begin
         if Assign_Timer.Elapsed < 1.0 or else not Shared_Context.Proceed then
            return;
         end if;
         Assign_Timer.Reset;

         Log ("Bot status is " & Bot_Status'Img & This.Bot.Get_Task_Count'Img,
              Trace.Debug, Log_Section);

         case Bot_Status is
            when Idle =>
               if not Shared_Context.Assignment.Get_Tasks (This.Bot.all).Is_Empty then
                  if Improv_Timer.Elapsed < 0.0 then -- Always false
                     Log ("Waiting for plan stabilization to proceed",
                          Trace.Debug, Log_Section);
                     return;
                  end if;

                  declare
                     procedure Do_It (Key   : in     Distributed.Object_Key;
                                      Value : in out Distributed.Object_Data'Class;
                                      Meta  : in     Distributed.Object_Metadata)
                     is
                        pragma Unreferenced (Key, Meta);
                        Context : Distributed.Types.Danneal renames
                          Distributed.Types.Danneal (Value);

                        Job     : constant Sancta.Tasks.Object'Class :=
                                    Context.Assignment.Get_Tasks
                                      (This.Bot.all).First_Element;
                     begin
                        --  If inconsistency, let it go:
                        if Local_Costs /= Cost_Proxy.Object (Context.Agent_Costs.Get) or else
                          Local_Plan /= Context.Plan
                        then
                           Log ("Delaying task execution, waiting for anneal sync...",
                                Warning, Log_Section);
                           return;
                        end if;

                        if Local_Costs.Contains (Prev_Task) and then
                          Local_Costs.Contains (Job.Get_Id)
                        then
--                           Log ("Prev/Curr" & Prev_Task'Img & Job.Get_Id'Img, Always);
                           Local_Costs.Add_To_Agent_Historic_Cost
                             (Name,
                              Local_Costs.Get_Cost
                                (Name, Prev_Task, Job.Get_Id));
                           Prev_Task := Job.Get_Id;
                           Log ("Historic cost is now " &
                             Sancta.Image
                               (Local_Costs.Get_Agent_Historic_Cost
                                  (Name)),
                             Informative);
                        else
                           Local_Costs.Set_Agent_Historic_Cost
                             (Name, 0.0);
                           Log ("Resetting history... missing tasks" &
                                Prev_Task'Img & Job.Get_Id'Img,
                                Warning, Log_Section);
                           Prev_Task     := Sancta.Tasks.No_Task;
                        end if;
                        Local_Costs.Set_Agent_Historic_Task (Name, Job);
                        Context.Agent_Costs.Set (Local_Costs);

                        Context.Plan.Mark_Task_Done (Job.Get_Id);
--                          Log ("Assigned task, new plan is:", Always);
--                          Context.Plan.Print_Tree_Summary;
                        This.Anneal.Set_Costs (Context.Agent_Costs.Get);
                        This.Anneal.Set_Tasks (Context.Plan);
                        Solver.Set_Initial_Solution (This.Anneal);

                        Curr_Task := Job.Get_Id;

                        This.Bot.Set_Tasks
                          (Context.Assignment.Get_Agent
                             (Name).Get_Tasks);
                        --  We assign all tasks, although we are only interested
                        --  in the execution of the first one.
                        --  This gives display advantages only.
                        Bot_Status := Executing;

                        --  Remove also from the active assignment and add it to
                        --  history ass.
                        declare
                           Ag    : Sancta.Agent.Object'Class  :=
                                     Context.Assignment.Get_Agent (Name);
                           Tasks : Task_Lists.List        := Ag.Get_Tasks;
                           First : Sancta.Tasks.Object'Class := Tasks.First_Element;
                        begin
                           Tasks.Delete_First;
                           Ag.Set_Tasks (Tasks);
                           Context.Assignment.Set_Agent (Ag);
                           Context.History.Add (Ag, First);
                        end;
                        Local_Ass := Context.Assignment;
                     end Do_It;

                     Ok      : Boolean;
                  begin
                     Database.Update (Shared_Context_Key, Do_It'Access, Ok);
                     if not Ok then
                        Log ("Unable to assign next task to robot.",
                             Warning, Log_Section);
                        raise Program_Error;
                     end if;
                  end;
               end if;
            when Executing =>
               if This.Bot.Get_Task_Count = 0 and then
                 Shared_Context.Assignment.Get_Tasks (This.Bot.all).Is_Empty and then
                 Prev_Task /= Sancta.Tasks.No_Task
               then
                  Log ("Execution ended with total cost " &
                       Sancta.Image
                         (Local_Costs.Get_Agent_Historic_Cost (Name)),
                       Informative);
                  Prev_Task := Sancta.Tasks.No_Task;
                  Bot_Status := Idle;
               elsif This.Bot.Get_Task_Count > 0 and then
                  not Is_In (This.Bot.Get_Tasks, Curr_Task)
               then
                  Log ("Finished execution of current task", Always, Log_Section);
                  Bot_Status := Idle;
                  This.Bot.Clear_Tasks;
                  --  Wait for the assignation of the next task.
               elsif This.Bot.Get_Task_Count = 0 and then
                  not Shared_Context.Assignment.Get_Tasks (This.Bot.all).Is_Empty then
                  Bot_Status := Idle;
                  Log ("No tasks, robot idle.", Always, Log_Section);
               elsif This.Bot.Get_Task_Count = 0 then -- ???
                  Bot_Status := Idle;
                  Log ("Idling of robot forced", Warning, Log_Section);
               end if;

               --  In case new tasks appeared, update running thing.
               if Bot_Status = Executing and then This.Bot.Get_Task_Count >= 1 then
                  if Is_In (This.Bot.Get_Tasks, Curr_Task) then
                     --  copy everything but after the Curr_Task
                     declare
                        Old_List : Task_Lists.List := This.Bot.Get_Tasks;
                        New_List : Task_Lists.List;
                        use type Task_Lists.List;
                     begin
                        loop
                           New_List.Append (Old_List.First_Element);
                           Old_List.Delete_First;
                           exit when New_List.Last_Element.Get_Id = Curr_Task;
                        end loop;
                        Task_Utils.Concatenate
                          (New_List,
                           Shared_Context.Assignment.Get_Agent (Name).Get_Tasks);
                        if New_List /= This.Bot.Get_Tasks then
                           This.Bot.Set_Tasks (New_List);
                           Log ("UPDATING robot tasks", Trace.Debug, Log_Section);
                        end if;
                     end;
                  else
                     --  copy everything, something strange happened:
                     declare
                        New_List : Task_Lists.List;
                        use type Task_Lists.List;
                     begin
                        New_List.Append (This.Bot.Get_First_Task);
                        Task_Utils.Concatenate
                          (New_List,
                           Shared_Context.Assignment.Get_Agent (Name).Get_Tasks);
                        if New_List /= This.Bot.Get_Tasks then
                           This.Bot.Set_Tasks (New_List);
                        end if;
                     end;
                  end if;
               end if;
         end case;
      end Assign_Tasks;

      --------------------
      -- Shared_Context --
      --------------------

      function Shared_Context return Distributed.Types.Danneal is
         use Distributed.Types;
      begin
         return Danneal (Database.Get (Shared_Context_Key));
      end Shared_Context;

      -------------------
      -- Evaluate_Here --
      -------------------

      function Evaluate_Here (Sol : in Sancta.Mutable_Assignment.Object)
                              return    Optimization.Cost
      is
         C : constant Sancta.Costs := Sol.Evaluate (This.Criterion);
      begin
         if C = Sancta.Infinite then
            return Optimization.Infinite;
         else
            return Optimization.Cost (C);
         end if;
      end Evaluate_Here;

      ------------------
      -- Check_Deaths --
      ------------------

      procedure Check_Deaths (Context : Distributed.Types.Danneal) is
         Deaths : Containers.String_Sets.Set;
         procedure Round_1 (I : Agent_Maps.Cursor) is
            Ag : constant String := Agent_Maps.Element (I).Get_Name;
         begin
            if Distributed.Aliveness.Lateness (Database.all, Ag) > Death_Lateness then
               Deaths.Include (Agent_Maps.Element (I).Get_Name);
            end if;
         end Round_1;
      begin
         Context.Agents.Iterate (Round_1'Access);

         if not Deaths.Is_Empty then
            declare
               procedure Do_It (Key   : in     Distributed.Object_Key;
                                Value : in out Distributed.Object_Data'Class;
                                Meta  : in     Distributed.Object_Metadata)
               is
                  pragma Unreferenced (Key, Meta);
                  Context : Distributed.Types.Danneal renames
                    Distributed.Types.Danneal (Value);

                  procedure Round_2 (I : Containers.String_Sets.Cursor) is
                     Ag    : constant String := Containers.String_Sets.Element (I);
                     Costs :          Cost_Proxy.Object renames
                       Cost_Proxy.Object (Context.Agent_Costs.Ref.all);
                  begin
                     if Context.History.Contains (Ag) and then
                       Context.History.Get_Agent (Ag).Has_Tasks then
                        Context.Plan.Mark_Task_Done
                          (Context.History.Get_Agent
                             (Ag).Get_Last_Task.Get_Id,
                           Done => False);
                        declare
                           Aux : Sancta.Agent.Object'Class :=
                                   Context.History.Get_Agent (Ag);
                           Tsk : Task_Lists.List := Aux.Get_Tasks;
                        begin
                           Tsk.Delete_Last;
                           Aux.Set_Tasks (Tsk);
                           Context.History.Set_Agent (Aux);
                        end;
                     end if;
                     Context.Agents.Exclude (Ag);
                     Costs.Exclude_Agent (Ag);
                     Log ("Marking agent " & Ag & " as DEAD",
                          Warning, Log_Section);
                     Context.Assignment.Set_Valid (False);
                     Context.Ass_Cost := Sancta.Infinite;
                  end Round_2;
               begin
                  Deaths.Iterate (Round_2'Access);
               end Do_It;

               Ok      : Boolean;
            begin
               Database.Update (Shared_Context_Key, Do_It'Access, Ok);
               if not Ok then
                  Log ("Unable to signal robot death.",
                       Warning, Log_Section);
                  raise Program_Error;
               end if;
            end;
         end if;
      end Check_Deaths;

      -------------------
      -- Check_Changes --
      -------------------

      procedure Check_Changes is
         use Distributed.Types;

         ---------------
         -- Add_Agent --
         ---------------

         procedure Add_Agent (I : in Agent_Maps.Cursor) is
         begin
            This.Anneal.Add_Agent (Agent_Maps.Element (I));
         end Add_Agent;

         -------------------
         -- Do_Better_Ass --
         -------------------

         procedure Do_Better_Ass (Key   : in     Distributed.Object_Key;
                                  Value : in out Distributed.Object_Data'Class;
                                  Meta  : in     Distributed.Object_Metadata)
         is
            pragma Unreferenced (Key, Meta);
            Dann : Danneal renames Danneal (Value);
         begin
            --  Discard our solution if there's some mismatch in the data
            if Local_Plan /= Dann.Plan or else
              This.Criterion /= Dann.Criterion or else
              Local_Agents /= Dann.Agents or else
              Local_Costs /= Cost_Proxy.Object (Dann.Agent_Costs.Ref.all) or else
              +Solver.Best_Cost >= Dann.Ass_cost
            then
               Log ("Discarding local best solution because of data mismatch",
                    Trace.Debug, Log_Section);
               return;
            end if;
            Dann.Found_By   := Network.Value (Name);
            Dann.Ass_Cost   := +Solver.Best_Cost;
            Log ("X2:" & S (Dann.Ass_Cost), Never);
            Dann.Assignment := Solver.Best_Solution.To_Assignment;
            Dann.Criterion  := This.Criterion;
            Local_Ass       := Dann.Assignment;
            Log ("New best solution shared", Trace.Debug, Log_Section);
         end Do_Better_Ass;

         Context : constant Danneal := Shared_Context;
         Ok      :          Boolean;
      begin
         if Changes_Cron.Elapsed < 1.0 then
            return;
         end if;
         Changes_Cron.Reset;

         if Context.Agents /= Local_Agents or else
           Context.Plan /= Local_Plan or else
           Cost_Proxy.Object (Context.Agent_Costs.Ref.all) /= Local_Costs or else
           This.Criterion /= Context.Criterion
         then
            Log ("Changes in annealing context [Ag/Pl/Cs/Cr]: " &
                 Boolean'Image (Context.Agents /= Local_Agents) & "/" &
                 Boolean'Image (Context.Plan /= Local_Plan) & "/" &
                 Boolean'Image (Cost_Proxy.Object (Context.Agent_Costs.Ref.all) /= Local_Costs) & "/" &
                 Boolean'Image (Context.Criterion /= This.Criterion),
                 Trace.Debug, Log_Section);

            This.Criterion := Context.Criterion;
            Local_Agents   := Context.Agents;
            This.Anneal.Clear_Agents;
            Local_Agents.Iterate (Add_Agent'Access);
            Local_Plan     := Context.Plan;
            if Local_Plan /= Context.Plan then
               Local_Plan.Print_Tree_Summary;
               Context.Plan.Print_Tree_Summary;
            end if;
            pragma Assert (Local_Plan = Context.Plan);
            Local_Ass      := Context.Assignment;
            Local_Costs    := Cost_Proxy.Object (Context.Agent_Costs.Get);
            pragma Assert (Local_Costs = Cost_Proxy.Object (Context.Agent_Costs.Get));
            This.Anneal.Set_Criterion (This.Criterion);
            This.Anneal.Set_Costs (Local_Costs);
            This.Anneal.Set_Tasks (Local_Plan);
            if This.Anneal.To_Assignment.Is_Valid then
               Solver.Set_Initial_Solution (This.Anneal);
            else
               --  Force a new attempt later:
               Local_Plan.Clear;
               Log ("Delaying annealing synch...", Warning, Log_Section);
            end if;
            Improv_Timer.Reset;
         elsif Solver.Best_Cost < Optimization.Infinite and then
            +Solver.Best_Cost < Context.Ass_Cost
         then
            Database.Update (Shared_Context_Key,
                             Do_Better_Ass'Access,
                             Ok,
                             Tout => 1.0);
            if not Ok then
               Log ("Failed Do_Better_Assignment", Warning, Log_Section);
            end if;
         end if;

         --  Aliveness things
         Distributed.Aliveness.Update (Database.all, Name);
         Check_Deaths (Context);

         --  Update poses as long as we are not executing
         if not Context.Proceed and then
           ((not Context.Agents.Contains (Name)) or else
            Agent_Proxy.Object
              (Context.Agents.Element (Name)).Get_Pose /= This.Bot.Get_Pose)
         then
            Update_Pose;
            Log ("Updating pose", Always);
         end if;

         if Local_Ass /= Context.Assignment then
            Local_Ass := Context.Assignment;
         end if;

      end Check_Changes;

      -----------------
      -- Update_Pose --
      -----------------

      procedure Update_Pose is
         procedure Do_It (Key   : in     Distributed.Object_Key;
                          Value : in out Distributed.Object_Data'Class;
                          Meta  : in     Distributed.Object_Metadata)
         is
            pragma Unreferenced (Key, Meta);
            use Distributed.Types;
            Context : Danneal renames Danneal (Value);
            Ag      : Agent_Proxy.Object;
         begin
            Ag.Set_Name (Name);
            Ag.Set_Pose (This.Bot.Get_Pose);
            Context.Agents.Include (Ag.Get_Name, Ag);
            Cost_Proxy.Object (Context.Agent_Costs.Ref.all).Clear_All_History;
            Cost_Proxy.Object (Context.Agent_Costs.Ref.all).Set_Agent (Ag);
            Context.Ass_Cost := Sancta.Infinite;
            Context.History.Clear;
            Log ("Pose updated succesfully", Trace.Debug, Log_Section);
         end Do_It;

         Ok      : Boolean;
      begin
         Database.Update (Shared_Context_Key,
                          Do_It'Access,
                          Ok, 1.0);
         if not Ok then
            Log ("Couldn't update pose", Warning, Log_Section);
         end if;
      end Update_Pose;

      --------------
      -- Progress --
      --------------
      Local_Best_Cost : Sancta.Costs := Sancta.Infinite;
      Local_Low       : Sancta.Costs := Sancta.Infinite;
      Local_High      : Sancta.Costs := 0.0;
      Timer_1s        : Agpl.Chronos.Object;
      Iters           : Natural := 0;
      procedure Progress (Continue : out Boolean) is
         use Optimization.Annealing;
      begin
         Continue := True;
         Iters    := Iters + 1;

         if +Solver.Best_Cost < Local_Best_Cost then
            Log ("Better local solution found: " &
                 S (Solver.Best_Cost) & " < " & S (Local_Best_Cost),
                 Trace.Debug, Log_Section);
            Local_Best_Cost := +Solver.Best_Cost;
         end if;

         Local_High := Sancta.Costs'Max (Local_High, +Solver.Current_Cost);
         Local_Low  := Sancta.Costs'Min (Local_Low, +Solver.Current_Cost);

         if Timer_1s.Elapsed >= 1.0 then
            Log ("Glob:" & S (Shared_Context.Ass_Cost) &
                 "; Local:" & S (Sancta.Costs (Solver.Best_Cost)) &
                 "; Curr:" & S (Solver.Current_Cost) &
                 "; Rng:" & S (Local_Low) & "-" & S (Local_High) &
                 "; T:" &
                 Solver.Current_Temperature'Img &
                 "; I/s:" &
                 Natural'Image
                   (Natural (Float (Iters) / Float (Timer_1s.Elapsed))),
                 Trace.Debug, Detail_Section);

            Timer_1s.Reset;
            Iters      := 0;
            Local_Low  := Sancta.Infinite;
            Local_High := 0.0;
         end if;

         declare
            Prev_Temp : constant Optimization.Annealing.Temperature :=
                          Man_Temp.Get_Temperature (0.0);
         begin
            Man_Temp.Update (Solver.Current_Cost);
            if Prev_Temp < Man_Temp.Get_Temperature (0.0) then
               --  On bump, show stats:
               Solver.Print_Stats;
               Solver.Reset_Stats;
            end if;
         end;
      end Progress;

      ----------------------------
      -- Do_Annealing_Iteration --
      ----------------------------

      procedure Do_Annealing_Iteration is
         Slice : constant Duration := 0.01;
      begin
         Solver.Work (Man_Temp.Get_Temperature'Access,
                      Iterations => Positive'Last,
                      Timeout    => Slice,
                      Converge   => 60.0,
                      Progress   => Progress'Access);
         delay Slice;
            --  Attempt at not starving the robot CPU...
      exception
         when E : others =>
            Log ("Trying to iterate: " & Report (E), Warning, Log_Section);
            delay 1.0;
      end Do_Annealing_Iteration;

      ------------------------------
      -- Initialize_Shared_Values --
      ------------------------------

      procedure Initialize_Shared_Values is
      begin
         while This.Bot.Get_Name = "" loop delay 0.1; end loop;
         Name := This.Bot.Get_Name;
         --  UHHH?????? The name should be up at this time!!!

         Distributed.Aliveness.Update (Database.all, Name);

         declare
            Success : Boolean;
            Context : Distributed.Types.Danneal; -- Default is not ok
            Costs   : Cost_Proxy.Object;
         begin
            --  Add empty cost cache
            Context.Agent_Costs.Set (Costs);
            Context.Criterion := This.Criterion;
            Context.Plan.Add_Method (Methods.Explore_Segment_Expansion.Instance);
            Database.Create (Shared_Context_Key, Context, Success);
            if Success then
               Log ("Shared context created", Informative, Log_Section);
            else
               Log ("Shared context already present", Informative, Log_Section);
            end if;
         end;

         Update_Pose;
      end Initialize_Shared_Values;

      Log_Timer : Chronos.Object;
      procedure Do_Logging is
      begin
         if This.Log and then Log_Timer.Elapsed >= 0.1 then
            Log_Timer.Reset;
            declare
               use Ada.Text_Io;
               F : File_Type;
            begin
               Open (F, Mode => Append_File, Name => Logfile);
               Put_Line (F,
                         S (Solver.Current_Temperature, 20) & " " &
                         S (Solver.Current_Cost) & " " &
                         S (Solver.Best_Cost));
               Close (F);
            end;
         end if;
      end Do_Logging;

   begin

      accept Start;

      Initialize_Shared_Values;

      --  Initialize annealing with empty objects or whatever we have:
      This.Anneal.Set_Costs (Local_Costs);
      This.Anneal.Set_Tasks (Local_Plan);
      This.Anneal.Set_Assignment (This.Anneal.To_Assignment, This.Criterion);
      if This.Anneal.To_Assignment.Is_Valid then
         Solver.Set_Initial_Solution (This.Anneal);
      end if;

      while not Done loop
         select
            accept Shutdown;
            Done := True;
         else
            null;
         end select;

         Check_Changes;

         Do_Annealing_Iteration;

         Assign_Tasks;

         Do_Logging;

      end loop;
      Log ("Active task shut down.", Trace.Debug, Log_Section);
   end Annealer_Task;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Annealer;
