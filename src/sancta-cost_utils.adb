--  with Sancta.Agent_Proxy;
--  with Sancta.Debug;
--  with Sancta.Draw_Mtsp;
--  with Sancta.Problems.City43;
--  with Sancta.Tasks.Entry_Point;
--  with Sancta.Tasks.Explore_Edge;
--  with Sancta.Tasks.Explore_Directed_Edge;
--  with Sancta.Tasks.Explore_Directed_Segment;
--  with Sancta.Tasks.Gaps_At_Pose;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Grid_Goal;
--  with Sancta.Tasks.Hold_Pose;
with Sancta.Tasks.Positioned;
--  with Sancta.Tasks.Track_Two_Points;
with Sancta.Tasks.Wander;
with Sancta.Tasks.Speed_Driving;
with Sancta.Traderbot_Sim;
with Sancta.Types.Operations;

with Sancta.Tasks.Starting_Pose;
--  with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;

package body Sancta.Cost_Utils is

   use type Ada.Containers.Count_Type;
   use type Sancta.Costs;
   use type Types.Real;

--    Cost_Adjust : constant Sancta.Costs := Sancta.Costs (Tasks.Explore_Edge.Cost_Adjust);

   --  For these functions, we require that the agents are of class
   --  Sancta.Agent_Proxy (to be able to get its pose).
   --  And the tasks all of type Positioned (same reason).
   --  So we can draw schemes of cost and position.

   ------------------------------
   -- Get_Concorde_Cost_Matrix --
   ------------------------------

--     function Get_Concorde_Cost_Matrix
--       (Agents : in Sancta.Agent.Containers.Lists.List;
--        Tasks  : in Sancta.Tasks.Containers.Lists.List;
--        Costs  : in Sancta.Cost_Matrix.Object) return Optimization.Concorde.Cost_Matrix
--     is
--        pragma Unreferenced (Agents, Tasks, Costs);
--     begin
--        raise Program_Error;
--
--        --  We have a problem and it is that all robots must share costs for all
--        --  the tasks to be able to solve this with concorde.
--
--        return Optimization.Concorde.Cost_Matrices.Create (First_Row => 2,
--                                                           Last_Row  => 1,
--                                                           First_Col => 2,
--                                                           Last_Col  => 1);
--        --  An empty matrix
--     end Get_Concorde_Cost_Matrix;

   ------------------------
   -- Get_Execution_Cost --
   ------------------------

   function Get_Execution_Cost (From, To  : in Sancta.Tasks.Object'Class;
                                Curr_Pose : in Types.Pose;
                                Lin_Speed,
                                Ang_Speed : in Types.Real)
                                return         Sancta.Costs
   is
--        use Tasks.Explore_Edge;
--        use Tasks.Explore_Directed_Edge;
--        use Tasks.Explore_Directed_Segment;
      use Types.Operations;

--        package TEDE renames Tasks.Explore_Directed_Edge;
      package TGG  renames Tasks.Grid_Goal;

      -------------------------
      -- Get_First_Task_Cost --
      -------------------------

      function Get_First_Task_Cost (Curr_Pose : in Types.Pose;
                                    Job       :  in Sancta.Tasks.Object'Class)
                                    return    Sancta.Costs
      is
      begin
         if False then
            return 0.0;
--           if Job in Tasks.Entry_Point.Object'Class then
--              return 0.0;
         elsif Job in Tasks.Goto_Pose.Object'Class and then
               not Tasks.Goto_Pose.Object (Job).Use_Angle
         then
            return Sancta.Costs
              (Time
                 (Curr_Pose,
                  Tasks.Positioned.Object (Job).Pose,
                  Lin_Speed,
                  Ang_Speed,
                  Use_A_Angle => True, Use_B_Angle => False));
         elsif Job in Tasks.Positioned.Object'Class then
            return Sancta.Costs (
              (Time (Curr_Pose,
                     Tasks.Positioned.Object (Job).Pose,
                     Lin_Speed => Lin_Speed,
                     Ang_Speed => Ang_Speed)));
         elsif Job in Tasks.Wander.Object'Class then
            return 0.0;
         elsif Job in Tasks.Speed_Driving.Object'Class then
            return Sancta.Costs (Tasks.Speed_Driving.Object (Job).Remains);
--           elsif Job in Tasks.Explore_Directed_Segment.Object'Class then
--              if Tasks.Explore_Directed_Segment.Object (Job).On_Segment then
--                 --  Just go to the end of the segment
--                 return Sancta.Costs (Types.Operations.Time
--                                         (Curr_Pose,
--                                          Get_To (Tasks.Explore_Directed_Segment.Object (To)),
--                                          Lin_Speed => Lin_Speed,
--                                          Ang_Speed => Ang_Speed));
--              else
--                 return Sancta.Costs (Types.Operations.Time -- Go to new segment
--                                         (Curr_Pose,
--                                          Get_From (Tasks.Explore_Directed_Segment.Object (To)),
--                                          Lin_Speed => Lin_Speed,
--                                          Ang_Speed => Ang_Speed) +
--                                         Types.Operations.Time -- Walk the segment
--                                           (Get_From (Tasks.Explore_Directed_Segment.Object (To)),
--                                            Get_To (Tasks.Explore_Directed_Segment.Object (To)),
--                                            Lin_Speed => Lin_Speed,
--                                            Ang_Speed => Ang_Speed));
--              end if;
--           elsif Job in Tasks.Explore_Directed_Edge.Object'Class then
--              if Tasks.Explore_Directed_Edge.Object (Job).On_Segment then
--                 --  Just go to the end of the segment
--                 return Sancta.Costs (Types.Operations.Time
--                                         (Curr_Pose,
--                                          Get_Pose (Get_To (Tasks.Explore_Directed_Edge.Object (To))),
--                                          Lin_Speed => Lin_Speed,
--                                          Ang_Speed => Ang_Speed)) * Cost_Adjust;
--              else
--                 declare
--                    Entry_Vertex : constant Tasks.Explore_Edge.Pose_Graphs.Vertex_Index :=
--                                     Problems.City43.Get_Entry_Vertex (Curr_Pose);
--                 begin
--                    --  Go to the closest entry point and from them follow best route
--                    --  Note that this is non-sensical for in progress missions, but is
--                    --  right for robots standing in entry points
--                 return
--                   Sancta.Costs (Types.Operations.Time -- Go to entry pose (probably there already)
--                                    (Curr_Pose,
--                                     Get_Pose (Entry_Vertex),
--                                     Lin_Speed, Ang_Speed)) * Cost_Adjust +
--                   Sancta.Costs (Tasks.Explore_Edge.Get_Cost -- Goto start of some segment
--                                    (Entry_Vertex,
--                                     Get_From (TEDE.Object (To)))) +
--                      Sancta.Costs (Tasks.Explore_Edge.Get_Cost -- And walk it
--                                       (Get_From (TEDE.Object (To)),
--                                        Get_To   (TEDE.Object (To))));
--                 end;
--              end if;
         elsif Job in Tasks.Grid_Goal.Object'Class then
            declare
               T : Tasks.Grid_Goal.Object renames Tasks.Grid_Goal.Object (Job);
               use Tasks.Grid_Goal;
            begin
               return Tasks.Grid_Goal.Get_Cost (Get_Cell (Curr_Pose),
                                                T.Get_Cell);
            end;
         else
            return Sancta.Infinite;
         end if;
      end Get_First_Task_Cost;

      Ini_Pose  : Types.Pose;
      Ini_Valid : Boolean;
   begin
--        Log ("Asked cost from " & From.To_String &
--             " to " & To.To_String, Always);

      --  TASKS ONLY VALID AS FIRST TASK!!
      if From in Sancta.Tasks.Starting_Pose.Object then
         return Get_First_Task_Cost (Curr_Pose, To);
      else
         --  COST FROM FINISHING PREVIOUS TASK TO FINISHING NEXT TASK!!

         --  Tasks that can't be a To:
         if False then
            return Sancta.Infinite;
         elsif -- To in Tasks.Entry_Point.Object or else
           To in Sancta.Tasks.Starting_Pose.Object
         then
            return Sancta.Infinite;
         end if;

         --  Prepare, just in case, Ini_Pose
         if From in Tasks.Positioned.Object'Class then
            Ini_Pose  := Tasks.Positioned.Object (From).Pose;
            Ini_Valid := True;
--           elsif From in Tasks.Explore_Directed_Segment.Object'Class then
--              Ini_Pose := Tasks.Explore_Directed_Segment.Object (From).Get_To;
--              Ini_Valid := True;
         end if;

            --  Now, really doable tasks:
         if Ini_Valid and then To in Tasks.Goto_Pose.Object'Class and then
               not Tasks.Goto_Pose.Object (To).Use_Angle
         then
            --  We'll simplify and discard rotations in both ini and fin pose:
            return Get_First_Task_Cost (Ini_Pose, To);
         elsif Ini_Valid and then To in Tasks.Positioned.Object'Class then
            return Sancta.Costs (Time
                                    (Ini_Pose,
                                     Tasks.Positioned.Object (To).Pose,
                                     Lin_Speed => Lin_Speed,
                 Ang_Speed => Ang_Speed));
         elsif To in Tasks.Speed_Driving.Object'Class then
            return Sancta.Costs (Tasks.Speed_Driving.Object (To).Remains);
--           elsif To in Tasks.Explore_Directed_Segment.Object then
--              if not Ini_Valid then
--                 return Sancta.Infinite; -- ?????? We don't know where we have finished From
--              else
--                 return Sancta.Costs (Types.Operations.Time -- Go to new segment
--                   (Ini_Pose,
--                      Get_From (Tasks.Explore_Directed_Segment.Object (To)),
--                      Lin_Speed => Lin_Speed,
--                      Ang_Speed => Ang_Speed) +
--                     Types.Operations.Time -- Walk the segment
--                       (Get_From (Tasks.Explore_Directed_Segment.Object (To)),
--                        Get_To (Tasks.Explore_Directed_Segment.Object (To)),
--                        Lin_Speed => Lin_Speed,
--                        Ang_Speed => Ang_Speed));
--              end if;
--           elsif To in Tasks.Explore_Directed_Edge.Object then
--              --  Expected case:
--              if From in Tasks.Explore_Directed_Edge.Object then
--                 return
--                   Sancta.Costs (Tasks.Explore_Edge.Get_Cost
--                                    (Get_To   (TEDE.Object (From)),
--                                     Get_From (TEDE.Object (To)))) +
--                   Sancta.Costs (Tasks.Explore_Edge.Get_Cost
--                                    (Get_From (TEDE.Object (To)),
--                                     Get_To   (TEDE.Object (To))));
--              elsif not Ini_Valid then
--                 return Sancta.Infinite;
--              else
--                 return Get_First_Task_Cost (Ini_Pose, To);
--                 --  Best approximation we can have...
--              end if;
         elsif To in Tasks.Grid_Goal.Object then
            --  Expected case:
            if From in Tasks.Grid_Goal.Object then
               return TGG.Get_Cost (TGG.Object (From).Get_Cell,
                                    TGG.Object (To).Get_Cell);
            elsif not Ini_Valid then
               return Sancta.Infinite;
            else
               return Get_First_Task_Cost (Ini_Pose, To);
               --  Best approximation we can have...
            end if;
         elsif To in Tasks.Wander.Object'Class then
            return 0.0;
         else
            return Sancta.Infinite;
         end if;
      end if;
   exception
      when E : Constraint_Error =>
         Log ("Get_Execution_Cost: Constraint error: " & Report (E), Warning);
         return Sancta.Infinite;
   end Get_Execution_Cost;

   ----------------------
   -- Get_Optimal_Cost --
   ----------------------

--     function Get_Optimal_Cost
--       (Agents : in Sancta.Agent.Containers.Lists.List;
--        Tasks  : in Sancta.Tasks.Containers.Lists.List) return Sancta.Costs
--     is
--        use Agpl.Optimization.Concorde;
--        use Sancta.Agent.Containers.Lists;
--        use Sancta.Tasks.Containers.Lists;
--
--        --  We'll use the final positions for robot's poses:
--
--        M : constant Salesmen := Salesmen (Agents.Length);
--        --  M is the number of travelers/agents.
--
--        N : constant Cities := Cities (Tasks.Length) + Cities (M);
--        --  N is the number of cities (tasks + starting places)
--
--        type CostR_Matrix is array (Integer range <>, Integer range <>) of Sancta.Costs;
--
--        Cost  : Cost_Matrices.Matrix := Cost_Matrices.Create (N, N);
--        CostR : CostR_Matrix (1 .. Integer (N), 1 .. Integer (N));
--        Start : Start_Matrix (1 .. M);
--
--        TaskV : Sancta.Tasks.Containers.Vectors.Vector;  --  We'll use a vector for simplicity.
--
--        Bot   : constant Agent_Proxy.Object :=
--                  Agent_Proxy.Object (Agents.First_Element);
--
--        --  Poses : Types.Pose_Array (1 .. Integer (N));
--     begin
--        --  Set up starting places:
--        for I in Start'Range loop
--           Start (I) := N - Cities (M) + Cities (I);
--        end loop;
--
--        --  Move tasks to the vector:
--        declare
--           procedure Append (X : in Sancta.Tasks.Containers.Lists.Cursor) is
--           begin
--              Sancta.Tasks.Containers.Vectors.Append (TaskV, Element (X));
--  --              Poses (Sancta.Tasks.Containers.Last_Index (TaskV)) :=
--  --                Sancta.Tasks.Positioned.Object (TaskV.Last_Element).Pose;
--           end Append;
--        begin
--           Sancta.Tasks.Containers.Lists.Iterate (Tasks, Append'Access);
--        end;
--
--        --  Add agent starting poses as task:
--        declare
--           procedure Append (X : in Sancta.Agent.Containers.Lists.Cursor) is
--           begin
--              Sancta.Tasks.Containers.Vectors.Append
--                (TaskV,
--                 Sancta.Tasks.Positioned.Object'
--                   (Sancta.Tasks.Object with Pose =>
--                      Agent_Proxy.Object (Element (X)).Get_Pose));
--  --              Poses (Sancta.Tasks.Containers.Last_Index (TaskV)) :=
--  --                Agent_Proxy.Object (Element (X)).Get_Pose;
--           end Append;
--        begin
--           Sancta.Agent.Containers.Lists.Iterate (Agents, Append'Access);
--        end;
--
--        pragma Assert (Cities (TaskV.Length) = N);
--
--        --  Compute costs:
--        for From in Cost.First_Row .. Cost.Last_Row loop
--           for To in Cost.First_Col .. Cost.Last_Col loop
--              if From = To then
--                 CostR (Integer (From), Integer (To)) := 0.0;
--                 Cost.Set (From, To, 0);
--              else
--                 CostR (Integer (From), Integer (To)) :=
--                   Bot.Get_Cost (TaskV.Element (Positive (From)),
--                                 TaskV.Element (Positive (To)));
--                 Cost.Set
--                   (From, To,
--                    Optimization.Concorde.Costs'Max
--                      (1, Optimization.Concorde.Costs
--                         (Costr (Integer (From), Integer (To)))));
--              end if;
--           end loop;
--        end loop;
--
--        Print_Problem (Cost);
--
--        --  Solve!
--        declare
--           Result : constant Result_Matrix :=
--                      Solve_MTSP (Start, Cost, No_Return => True);
--           Sum    : Sancta.Costs := 0.0;
--        begin
--           Log ("OPTIMAL SOLUTION: ", Informative);
--           Log ("#Agents:" & Agents.Length'Img, Informative);
--           Print_Solution (Cost, Start, Result, No_Return => True);
--
--           --  Compute the real cost without roundings to Integer:
--           for I in Result'Range loop
--              declare
--                 Tour : constant Result_Matrix := Normalize_Tour (N - Cities (M) + Cities (I),
--                                                                  Result);
--              begin
--                 for J in Tour'First (2) .. Tour'Last (2) - 1 loop
--                    Sum := Sum + CostR (Integer (Tour (1, J)),
--                                        Integer (Tour (1, J + 1)));
--                 end loop;
--              end;
--           end loop;
--
--           Log ("Real optimal cost incurred: " & Debug.To_String (Sum),
--                Informative);
--
--           return Sum;
--        end;
--     end Get_Optimal_Cost;

   ---------------------
   -- Get_Trader_Cost --
   ---------------------

   function Get_Trader_Cost
     (Agents        : in Sancta.Agent.Containers.Lists.List;
      Tasks         : in Sancta.Tasks.Containers.Lists.List;
      Task_Policy   : in Auctions.Auction_Task_Policies;
      Insert_Policy : in Auctions.Insertion_Policies;
      Criterion     : in Criteria.Assignment_Criteria) return Sancta.Costs
   is
      pragma Unreferenced (Task_Policy, Insert_Policy);
      use Sancta.Agent.Containers.Lists;
      use Sancta.Tasks.Containers.Lists;

      Sim : Traderbot_Sim.Object;
   begin
      Sim.Set_Criterion (Criterion);

      declare
         I : Sancta.Agent.Containers.Lists.Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            Sim.Add_Agent (Element (I));
            Next (I);
         end loop;
      end;

      declare
         I : Sancta.Tasks.Containers.Lists.Cursor := Tasks.First;
      begin
         while Has_Element (I) loop
            Sim.Add_Task (Element (I));
            Next (I);
         end loop;
      end;

      return Sim.To_Assignment.Get_Cost (Criterion);
   end Get_Trader_Cost;

   --------------------------
   -- Replace_Initial_Cost --
   --------------------------

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Commited_Task : in     Sancta.Tasks.Task_Id;
                                   Tasks         : in     Task_Lists.List)
   is
      use Task_Lists;

      Fixed_Cost : constant Sancta.Costs :=
                     Sancta.Cost_Matrix.Get_Cost (Costs,
                                              Agent.Get_Name,
                                              Sancta.Tasks.No_Task,
                                              Commited_Task);

      procedure Do_It (I : Cursor) is
         C : constant Sancta.Costs := Sancta.Cost_Matrix.Get_Cost (Costs,
                                                           Agent.Get_Name,
                                                           Commited_Task,
                                                           Element (I).Get_Id);
      begin
         Sancta.Cost_Matrix.Set_Cost (Costs,
                                  Agent.Get_Name,
                                  Sancta.Tasks.No_Task,
                                  Element (I).Get_Id,
                                  Fixed_Cost + C);
      end Do_It;
   begin
      Tasks.Iterate (Do_It'Access);
   end Replace_Initial_Cost;

   --------------------------
   -- Replace_Initial_Cost --
   --------------------------

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Tasks         : in     Task_Lists.List)
   is
      use Task_Lists;

      Fixed_Cost     : constant Sancta.Costs := Agent.Get_Plan_Cost;

      procedure Do_It (I : Cursor) is
--           Prev : constant Sancta.Costs :=
--                    Sancta.Cost_Matrix.Get_Cost (Costs,
--                                             Agent.Get_Name,
--                                             Sancta.Tasks.No_Task,
--                                             Element (I).Get_Id);
      begin
         if Agent.Has_Tasks then
            Sancta.Cost_Matrix.Set_Cost
              (Costs,
               Agent.Get_Name,
               Sancta.Tasks.No_Task,
               Element (I).Get_Id,
               Fixed_Cost + Agent.Get_Cost
                 (Agent.Get_Tasks.Last_Element,
                  Element (I)));
         else
            Sancta.Cost_Matrix.Set_Cost
              (Costs,
               Agent.Get_Name,
               Sancta.Tasks.No_Task,
               Element (I).Get_Id,
               Fixed_Cost + Agent.Get_Cost (Element (I)));
         end if;
--           Log ("Replaced for 0:" & Element (I).Get_Id'Img & ": " &
--             Prev'Img & Sancta.Cost_Matrix.Get_Cost
--               (Costs,
--                Agent.Get_Name,
--                Sancta.Tasks.No_Task,
--                Element (I).Get_Id)'Img,
--             Always);
      end Do_It;
   begin
      Tasks.Iterate (Do_It'Access);
   end Replace_Initial_Cost;

   --------------------------
   -- Replace_Initial_Cost --
   --------------------------

   procedure Replace_Initial_Cost (Agent         : in     Sancta.Agent.Object'Class;
                                   Costs         : in out Sancta.Cost_Matrix.Object;
                                   Historic_Cost : in     Sancta.Costs;
                                   Commited_Task : in     Sancta.Tasks.Task_Id;
                                   Tasks         : in     Task_Lists.List)
   is
      use Task_Lists;
      procedure Do_It (I : Cursor) is
--           Prev : constant Sancta.Costs :=
--                    Sancta.Cost_Matrix.Get_Cost (Costs,
--                                             Agent.Get_Name,
--                                             Sancta.Tasks.No_Task,
--                                             Element (I).Get_Id);
      begin
         Sancta.Cost_Matrix.Set_Cost
           (Costs,
            Agent.Get_Name,
            Sancta.Tasks.No_Task,
            Element (I).Get_Id,
            Historic_Cost + Sancta.Cost_Matrix.Get_Cost
              (Costs,
               Agent.Get_Name,
               Commited_Task,
               Element (I).Get_Id));

--           Log ("Replaced for 0:" & Element (I).Get_Id'Img & ": " &
--             Prev'Img & Sancta.Cost_Matrix.Get_Cost
--               (Costs,
--                Agent.Get_Name,
--                Sancta.Tasks.No_Task,
--                Element (I).Get_Id)'Img,
--             Always);
      end Do_It;
   begin
      Tasks.Iterate (Do_It'Access);
   end Replace_Initial_Cost;

end Sancta.Cost_Utils;
