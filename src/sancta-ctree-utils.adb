with Ada.Containers.Ordered_Sets,
     Sancta.Tasks.Handle,
     Sancta.Tasks.Positioned,
     Sancta.Tasks.Utils;

   --  with Sancta.Ctree.Data_Source.Yarp;
with Sancta.Debug2;
with Sancta.Tasks.Navigate_To_Pose;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types.Operations;

with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Agent.Containers;
with Sancta.Tasks.Insertions;
--  with Agpl.Random;
use Agpl;

package body Sancta.Ctree.Utils is

   use type Types.Real;
   package Posed renames Sancta.Tasks.Positioned;
   package Ptt renames Path_Trees.Trees;

   type Path_Job is record
      Path : Map.Path;
      Pose : Types.Pose;
      Job  : Tasks.Handle.Object;
   end record;

   function Less_Cc (P1, P2 : Path_Job) return Boolean;

   package Sorted_Paths is new Ada.Containers.Ordered_Sets
     (Path_Job, Less_Cc);

   -----------
   -- Merge --
   -----------

   function Merge (L, R : Path_Trees.Node_Data'Class)
                   return Path_Trees.Node_Data'Class is
      use Tasks.Utils;
      Z : Node_Data := Node_Data (L);
   begin
      Append (Z.Tasks, Node_Data (R).Tasks);
      return Z;
   end Merge;

   procedure Merge is new Path_Trees.Trees.Merge (Merge);

   -----------------
   -- Less_Costly --
   -----------------

   function Less_Costly (L, R : Path_Trees.Cursor) return Boolean is
   begin
      return
        Node_Data (L.Query.all).Min_Branch_Cost <
        Node_Data (R.Query.all).Min_Branch_Cost;
   end Less_Costly;

   -----------
   -- Image --
   -----------

--     function Image (This : Path_Trees.Node_Data'Class) return String is
--        Node : Node_Data renames Node_Data (This);
--     begin
--        return
--          Node.Cost_From_Root'Img &
--        Node.Min_Branch_Cost'Img &
--        Node.Tasks.Length'Img;
--     end Image;

   -------------
   -- Less_Cc --
   -------------
   --  Paths ordered in CC order
   function Less_Cc (P1, P2 : Path_Job) return Boolean is
--        use Map.Bitmap;
      use Map;
      use Map.Paths;
      L : Cursor := Next (P1.Path.First);
      R : Cursor := Next (P2.Path.First);
   begin
      while Has_Element (L) and then Has_Element (R) loop
         declare
            Dl : constant Relative_Position'Class :=
                   Element (Previous (L)).Position (Element (L));
            Dr : constant Relative_Position'Class :=
                   Element (Previous (R)).Position (Element (R));
         begin
            if Dl < Dr then
               return True;
            elsif Dl = Dr then
               Next (L);
               Next (R);
            else
               return False;
            end if;
         end;
      end loop;

      if not (Has_Element (L) or Has_Element (R)) then
         return
           P1.Pose.X < P2.Pose.X or else
           (P1.Pose.X = P2.Pose.X and then P1.Pose.Y < P2.Pose.Y);
      else
         return Has_Element (R);
      end if;
   end Less_Cc;

   -------------
   -- Rel_Pos --
   -------------

   function Rel_Pos (Ini, Fin : Map.Location'Class)
                     return Map.Relative_Position'Class
   is
   begin
      return Ini.Position (Fin);
   end Rel_Pos;

   --------------
   -- Preorder --
   --------------

   function Preorder
     (Tree  : Tree_Navigator.Object'Class;
      Tasks : Tc.Lists.List)
      return Tc.Lists.List
   is
      use Tc.Lists;

      Sorted : Sorted_Paths.Set;
      Result : Tc.Lists.List;

      procedure Sort_Paths (I : Cursor) is
         Path : constant Map.Path := Tree.Branch (Element (I));
      begin
         Sorted.Include
           ((Path,
             Posed.Object (Element (I)).Pose,
             Sancta.Tasks.Handle.Set (Element (I))));
      end Sort_Paths;

      procedure Enumerate_Paths (I : Sorted_Paths.Cursor) is
         use Sorted_Paths;
      begin
         Result.Append (Element (I).Job.Get);
      end Enumerate_Paths;
   begin
      Tasks.Iterate (Sort_Paths'Access);
      Sorted.Iterate (Enumerate_Paths'Access);

      return Result;
   end Preorder;

   -------------------------
   -- Depth_Closest_First --
   -------------------------

   function Depth_Closest_First
     (Tree  : Tree_Navigator.Object'Class;
      Tasks : Tc.Lists.List) return Tc.Lists.List
   is
      Ordered_Tasks : Tc.Lists.List;

      procedure Build_Ordered_Tasks (T : Path_Trees.Tree) is
         procedure Iterate_By_Cost is
           new Path_Trees.Trees.Iterate_Ordered_Children (Less_Costly);
         procedure Check_Node (Node : Path_Trees.Cursor) is
            Data : Node_Data renames Node_Data (Node.Query.all);
         begin
            if not Data.Tasks.Is_Empty then
               Log ("Visiting: " &
                    Data.Cost_From_Root'Img &
                    " (" & Data.Min_Branch_Cost'Img & ") - " &
                    Data.Tasks.First_Element.Image,
                    Informative, Log_Section);
               Sancta.Tasks.Utils.Append (Ordered_Tasks, Data.Tasks);
            end if;
            Iterate_By_Cost (Node, Check_Node'Access);
         end Check_Node;
      begin
         Check_Node (Path_Trees.Cursor (T.Root));
      end Build_Ordered_Tasks;

      T : Path_Trees.Tree;
   begin
      Log ("At Depth_Closest_First...", Debug, Log_Section);

      Build_Tree (T, Tree, Tasks);
      Log ("Tree built", Debug, Log_Section);

      Build_Costs (Path_Trees.Cursor (T.Root));
      Log ("Costs built", Debug, Log_Section);

--        Print (T);

      Build_Ordered_Tasks (T);
      Log ("Order built", Debug, Log_Section);

      pragma Assert (Natural (Tasks.Length) = Natural (Ordered_Tasks.Length));

      return Ordered_Tasks;
   end Depth_Closest_First;

   ----------------
   -- Build_Tree --
   ----------------

   procedure Build_Tree
     (Tree  : out Path_Trees.Tree;
      Nav   :     Tree_Navigator.Object'Class;
      Tasks :     Tc.Lists.List)
   is
      use Tc.Lists;
      procedure Add_To_Tree (I : Cursor) is
         Job   : constant Sancta.Tasks.Object'Class := Element (I);
         Local :          Path_Trees.Trees.Tree;
         Prev_Node : Ptt.Cursor := Ptt.Cursor (Local.Root);
         Prev      : Map.Paths.Cursor;
         use Map.Paths;
         procedure Add_To_Tree (I : Map.Paths.Cursor) is
            Node : Node_Data;
         begin
            Node.Loc.Set (Element (I));
            if not Has_Element (Next (I)) then
               Node.Tasks.Append (Job);
            end if;
            if not Has_Element (Previous (I)) then
               Local.Root.Insert (Node);
               Prev_Node := Ptt.Cursor (Local.Root);
            else
               Prev_Node.Child
                 (Rel_Pos (Element (Prev), Node.Loc.Get)).Insert (Node);
               Prev_Node :=
                 Prev_Node.Child (Rel_Pos (Element (Prev), Node.Loc.Get));
            end if;
            Prev := I;
         end Add_To_Tree;
      begin
--           Log ("Adding path:", Debug, Log_Section);
--           Log (Map.Image (Nav.Branch (Element (I))), Debug, Log_Section);
         Nav.Branch (Element (I)).Iterate (Add_To_Tree'Access);
--           Log ("Global tree before", Always);
--           Print (Tree);
--           Log ("Local tree before", Always);
--           Print (Local);
         Merge (Tree, Local);
--           Log ("Global tree after merge", Always);
--           Print (Tree);
      end Add_To_Tree;
   begin
      Tasks.Iterate (Add_To_Tree'Access);
   end Build_Tree;

   -----------------
   -- Build_Costs --
   -----------------

   procedure Build_Costs (Node : Path_Trees.Cursor) is
   begin
      --  Build costs in the way down
      if Node.Is_Root then
         Node_Data (Node.Update.all).Cost_From_Root := 0.0;
      else
         declare
            Parent  : Node_Data renames Node_Data (Node.Parent.Query.all);
            Current : Node_Data renames Node_Data (Node.Update.all);
         begin
            Current.Cost_From_Root :=
              Parent.Cost_From_Root +
                Parent.Loc.Ref.Real_Cost (Current.Loc.Ref.all);
            Log ("Going down cost:" & Current.Cost_From_Root'Img,
                 Debug, Det_Section);
         end;
      end if;

      --  Follow on childs
      declare
         procedure Go_Down (I : Path_Trees.Cursor) is
         begin
            Build_Costs (I);
         end Go_Down;
      begin
         Node.Iterate_Children (Go_Down'Access);
      end;

      --  Build min-branch on the way up
      if not Node.Has_Children then
         Node_Data (Node.Update.all).Min_Branch_Cost :=
           Node_Data (Node.Query.all).Cost_From_Root;
      else
         declare
            Min   : Costs := Infinite;
            procedure Find_Min (I : Path_Trees.Cursor) is
               Child : Node_Data renames Node_Data (I.Query.all);
            begin
               Min := Costs'Min (Min, Child.Min_Branch_Cost);
            end Find_Min;
         begin
            Node.Iterate_Children (Find_Min'Access);
            Node_Data (Node.Update.all).Min_Branch_Cost := Min;
         end;
      end if;
   end Build_Costs;

   ---------------
   -- Tree_Cost --
   ---------------

   function Tree_Cost (Tree : Path_Trees.Tree) return Sancta.Costs is
      Cost : Sancta.Costs := 0.0;

      procedure Add_Children (I : Path_Trees.Cursor) is
         Node : constant Node_Data := Node_Data (I.Element);
         pragma Assert (Node.Cost_From_Root > 0.0);
      begin
         Cost := Cost + Node.Cost_From_Root -
                        Node_Data (I.Parent.Element).Cost_From_Root;
      end Add_Children;

   begin
      Tree.Root.Iterate_Children (Add_Children'Access);
      return Cost;
   end Tree_Cost;

--  with Ada.Numerics; use Ada.Numerics;
--  with Ada.Numerics.Elementary_Functions;
--  use Ada.Numerics.Elementary_Functions;

   use type Types.Pose;

   package AC renames Sancta.Agent.Containers;
   package Navigate renames Sancta.Tasks.Navigate_To_Pose;

--     function "+" (X : Player.Point_2d) return Sancta.Types.Pose is
--     begin
--        return (Sancta.Types.Real (X.X), Sancta.Types.Real (X.Y), 0.0);
--     end "+";
--     function "+" (X : Sancta.Types.Pose) return Player.Point_2d is
--     begin
--        return (Standard.Player.C_Float (X.X), Standard.Player.C_Float (X.Y));
--     end "+";

   -------------------
   -- Check_Reached --
   -------------------

   procedure Check_Reached (Ass   : in out Sancta.Assignment.Object;
                            Dist  :        Types.Real)
   is
      Agents : constant AC.Lists.List := Ass.Get_Agents;
      procedure Check (I : Ac.Lists.Cursor) is
         Bot : constant Robot.Object'Class :=
                 Robot.Object'Class (Ac.Lists.Element (I));
         Robot_Pose : constant Types.Pose := Bot.Get_Pose;
      begin
         if Bot.Has_Tasks then
            declare
               Task_Pose : constant Types.Pose :=
                             Sancta.Tasks.Positioned.Object'Class
                               (Bot.Get_First_Task).Pose;
            begin
               if Types.Operations.Distance (Robot_Pose, Task_Pose) <= Dist then
                  --  Data_Source.Yarp.Send_Goal_Reached (Value (Bot.Get_Name));
                  null;
                  pragma Yarp_Missing;
               end if;
            end;
         end if;
      end Check;
   begin
      Agents.Iterate (Check'Access);
   end Check_Reached;

   --------------------
   -- Remove_Reached --
   --------------------

   procedure Remove_Reached
     (Ass   : in out Sancta.Assignment.Object;
      Tasks : in out Sancta.Tasks.Containers.Lists.List;
      Dist  :        Types.Real;
      Done  :    out Natural)
   is
      Agents : AC.Lists.List   := Ass.Get_Agents;
      I      : AC.Lists.Cursor := Agents.First;
   begin
      Done := 0;
      while AC.Lists.Has_Element (I) loop
         declare
            Agent_Done : Natural := 0;
            Agent      : Located_Agent.Object'Class :=
                           Located_Agent.Object (Ac.Lists.Element (I));
         begin
            Remove_Reached (Agent, Tasks, Dist, Agent_Done);
            if Agent_Done > 0 then
               Done := Done + Agent_Done;
               Agents.Replace_Element (I, Agent);
            end if;
         exception
            when Constraint_Error =>
               null; -- Task was not one of the originals; nothing to do.
         end;
         AC.Lists.Next (I);
      end loop;
      if Done > 0 then
         Ass.Set_Agents (Agents);
      end if;
   end Remove_Reached;

   --------------------
   -- Remove_Reached --
   --------------------

   procedure Remove_Reached
     (Agent : in out Located_Agent.Object'Class;
      Tasks : in out Sancta.Tasks.Containers.Lists.List;
      Dist  :        Types.Real;
      Done  :    out Natural)
   is
      Robot_Pose : constant Types.Pose := Agent.Get_Pose;
   begin
      Done := 0;
      if Agent.Has_Tasks then
         declare
            Task_Pose : constant Types.Pose :=
              Sancta.Tasks.Positioned.Object'Class (Agent.Get_First_Task).Pose;
            Id        : constant Sancta.Tasks.Task_Id := Agent.Get_First_Task.Get_Id;
         begin
--              if not Sancta.Tasks.Insertions.Contains (Tasks, Id) then
--                 Done := True;
--                 Agent.Remove_First_Task;
--                 Log ("Agent " & Agent.Get_Name & " abandoned redundant pose " &
--                      Debug2.To_String (Task_Pose), Debug);
            if Types.Operations.Distance (Robot_Pose, Task_Pose) <= Dist then
               Done := Done + 1;
               Agent.Remove_First_Task;
               Sancta.Tasks.Insertions.Remove (Tasks, Id);
               Log ("Agent " & Agent.Get_Name & " reached goal " &
                    Debug2.To_String (Task_Pose), Informative);
            else
               Log ("Agent " & Agent.Get_Name & " is at distance " &
                    To_String
                      (Float (Types.Operations.Distance (Robot_Pose, Task_Pose))),
                    Never);
            end if;
         end;
      end if;
   end Remove_Reached;

--     ----------------------
--     -- Create_Obstacles --
--     ----------------------
--
--     function Create_Obstacles (Options : Config.Object)
--                                return    Obstacles.Arrays.C_Array
--     is
--        Obsts : Obstacles.Arrays.C_Array (1 .. Options.Value (Config.Num_Obstacles));
--        Rnd   : Random.Object;
--
--        use Sancta.Types; use Operations;
--
--        X         : constant Float := Options.Value (Config.Random_Tasks_X);
--        Y         : constant Float := Options.Value (Config.Random_Tasks_Y);
--        Xmin      : constant Float := Options.Value (Config.Random_Tasks_Xmin);
--        Ymin      : constant Float := Options.Value (Config.Random_Tasks_Ymin);
--        Max_Range : constant Float := Options.Value (Config.Reachable_Radius);
--
--        function Colliding (Pose : Sancta.Types.Pose;
--                            Last : Natural) return Boolean
--        is
--        begin
--           for I in Obsts'First .. Last loop
--              if Distance (Pose, +Obsts (I)) < +Options.value (Config.Springs_R)/2.0 then
--                 return True;
--              end if;
--           end loop;
--
--           return False;
--        end Colliding;
--     begin
--        Rnd.Reset (Options.Value (Config.Random_Seed));
--
--        for I in Obsts'Range loop
--           loop
--              declare
--                 Obs : constant Sancta.Types.Pose :=
--                         (+Rnd.Get_Float (X - Max_Range, X + Max_Range),
--                          +Rnd.Get_Float (Y - Max_Range, Y + Max_Range),
--                          0.0);
--              begin
--                 if Distance (Types.Origin, Obs) <= +Max_Range and then
--                   abs Obs.X >= 2.0 and then abs Obs.Y >= 2.0 and then
--                   Obs.X >= +Xmin and then Obs.Y >= +Ymin and then
--                   (not Colliding (Obs, I - 1)) and then
--                   Arctan (Y => +Obs.Y, X => +Obs.X) <=
--                   Options.Value (Config.Random_Tasks_Max_Angle)
--                 then
--                    Obsts (I) := +Obs;
--                    Log ("Obstacle created at " & Obsts (I).X'Img & Obsts (I).Y'Img,
--                         Never);
--                    exit;
--                 end if;
--              end;
--           end loop;
--        end loop;
--
--        return Obsts;
--     end Create_Obstacles;
--
--     ------------------
--     -- Create_Goals --
--     ------------------
--
--     function Create_Goals (Options : Config.Object;
--                            Obsts   : Obstacles.Arrays.C_Array)
--                            return    TC.Lists.List
--     is
--        use Sancta.Types;
--        use Types.Operations;
--        use Tc.Lists;
--
--        Tasks        : List;
--        Rnd          : Random.Object;
--
--        --  Detect goals to close to an obstacle
--        function Colliding (Goal : Sancta.Tasks.Goto_Pose.Object) return Boolean is
--        begin
--           for I in Obsts'Range loop
--              if Distance (Goal.Pose, +(Obsts (I))) < 1.0 then
--                 return True;
--              end if;
--           end loop;
--
--           return False;
--        end Colliding;
--     begin
--        Rnd.Reset (Options.Value (Config.Random_Seed));
--
--        if Options.Value (Config.Random_Tasks) then
--
--           declare
--              Xbase     : constant Float := Options.Value (Config.Random_Tasks_X);
--              Ybase     : constant Float := Options.Value (Config.Random_Tasks_Y);
--              Max_Range : constant Float := Options.Value (Config.Reachable_Radius);
--              Num_Clust : constant Integer := Options.Value (Config.Task_Clusters);
--              Clusters  : array (1 .. Num_Clust) of Types.Pose;
--           begin
--              case Num_Clust is
--                 when 1 =>
--                    Clusters (1) := (+Xbase, +Ybase, 0.0);
--                 when others =>
--                    raise Program_Error with "unimplemented";
--              end case;
--
--              --  Create tasksks
--              while Natural (Tasks.Length) < Options.Value (Config.Num_Tasks) loop
--                 if Options.Get_Map.Is_Null then
--                    declare
--                       Clus_Rng  : constant Float :=
--                                     Max_Range / Float (2 ** (Num_Clust - 1));
--                       Cluster   : constant Positive :=
--                                     Rnd.Get_Integer (Clusters'First, Clusters'Last);
--                       Goal      : constant Goto_Pose.Object :=
--                                     Goto_Pose.Create
--                                       ((+(Float (Clusters (Cluster).X) +
--                                          Rnd.Get_Float (-Clus_Rng, Clus_Rng)),
--                                        +(Float (Clusters (Cluster).Y) +
--                                            Rnd.Get_Float (-Clus_Rng, Clus_Rng)), 0.0));
--                    begin
--                       if Distance (Types.Origin, Goal.Pose) <= +Max_Range and then
--                         abs Goal.Pose.X >= 2.0 and then abs Goal.Pose.Y >= 2.0 and then
--                         Goal.Pose.X >= +Options.Value (Config.Random_Tasks_Xmin) and then
--                         Goal.Pose.Y >= +Options.Value (Config.Random_Tasks_Ymin) and then
--                         (not Colliding (Goal)) and then
--                         Arctan (Y => +Goal.Pose.Y, X => +Goal.Pose.X) <= Options.Value (Config.Random_Tasks_Max_Angle)
--                       then
--                          Tasks.Append (Goal);
--                       end if;
--                    end;
--                 else -- Random goals in a grid
--                    declare
--                       Map_Ratio : constant Float := Options.Value (Config.Map_Cellsize);
--                       use type Map.Bitmap.Terrains;
--
--                       M : Map.Bitmap.Object'Class renames
--
--                         Map.Bitmap.Object'Class (Options.Get_Map.Ref.all);
--                       X         : constant Abscissa := Abscissa
--                         (Rnd.Get_Integer
--                            (Integer (M.Get_Data'First (2)),
--                             Integer (M.Get_Data'Last (2))));
--                       Y : constant Ordinate := Ordinate
--                         (Rnd.Get_Integer
--                            (Integer (M.Get_Data'First (1)),
--                             Integer (M.Get_Data'Last (1))));
--                       P : constant Types.Pose :=
--                                     Map.Bitmap.To_Pose (Map_Ratio, (M'Access,
--                                                                     X => X,
--                                                                     Y => Y));
--                       G         : constant Goto_Pose.Object := Goto_Pose.Create (P);
--
--                       function Other_At (P : Types.Pose) return Boolean
--                       is
--                          I : Cursor := Tasks.First;
--                       begin
--                          while Has_Element (I) loop
--                             if Goto_Pose.Object (Element (I)).Pose = P then
--                                return True;
--                             end if;
--                             Next (I);
--                          end loop;
--                          return False;
--                       end Other_At;
--                    begin
--                       if (not Colliding (G)) and then
--                         M.Get_At (X, Y) = Map.Bitmap.Free and then
--                         (not Other_At (P)) and then
--                         X > 1 and then Y > 1
--                       then
--                          Tasks.Append (G);
--                       end if;
--                    end;
--                 end if;
--              end loop;
--           end;
--        else
--           --  Use the ones in the config file
--           Tasks := Options.Get_Tasks;
--        end if;
--
--        --  Adjust base
--        declare
--           Adj_Pose : constant Types.Pose :=
--             (+Options.Value (Config.Xbase), +Options.Value (Config.Ybase), 0.0);
--           I : Cursor := Tasks.First;
--           procedure Adjust_Base (T : in out Sancta.Tasks.Object'Class) is
--              P : Sancta.Tasks.Positioned.Object'Class renames
--                Sancta.Tasks.Positioned.Object'Class (T);
--           begin
--              P.Pose := P.Pose + Adj_Pose;
--           end Adjust_Base;
--        begin
--           while Has_Element (I) loop
--              Tasks.Update_Element (I, Adjust_Base'Access);
--              Next (I);
--           end loop;
--        end;
--
--        return Tasks;
--     end Create_Goals;
--
--     --------------------------
--     -- Estimate_Bots_Needed --
--     --------------------------
--
--     function Estimate_Bots_Needed (Goal    : Tasks.Positioned.Object'Class;
--                                    From    : Types.Pose;
--                                    Options : Config.Object) return Positive
--     is
--        --  The - 0.5 is because that's the maximum TOTAL excesss distance
--        --  that springs allow with 8124 settings. (*Not per spring*)
--        Est : constant Positive :=
--                Natural
--                  (Types.Real'Ceiling
--                     (Types.Operations.Distance (From, Goal.Get_Pose) /
--                        Types.Real (Options.Value (Config.Springs_R))));
--     begin
--        --  Log ("Estimate:" & Est'Img, Always);
--        return Est;
--     end Estimate_Bots_Needed;

--     --------------------
--     -- Tasks_In_Range --
--     --------------------
--
--     function Tasks_In_Range (Tasks : Tc.Lists.List;
--                              Stage : Positive;
--                              Opts  : Config.Object) return Tc.Lists.List
--     is
--        Result   :          Tc.Lists.List;
--
--        procedure Check (I : Tc.Lists.Cursor) is
--           Job : constant Sancta.Tasks.Positioned.Object :=
--                   Sancta.Tasks.Positioned.Object (Tc.Lists.Element (I));
--        begin
--           if Utils.Estimate_Bots_Needed (Job,
--                                          Sancta.Types.Origin,
--                                          Opts) <= Stage
--           then
--              Result.Append (Job);
--           end if;
--        end Check;
--     begin
--        Tasks.Iterate (Check'Access);
--        return Result;
--     end Tasks_In_Range;

   --------------------------
   -- Apply_Goal_Transform --
   --------------------------

   function Apply_To_Simple_Goal_Transform
     (Ass  : Sancta.Assignment.Object)
      return Sancta.Assignment.Object
   is
      Result : Sancta.Assignment.Object;

      procedure Check (I : Ac.Lists.Cursor) is
         Ag : constant Agent.Object'Class := Ac.Lists.Element (I);
      begin
         if Ag.Has_Tasks then
            declare
               T : constant Tasks.Object'Class := Ag.Get_First_Task;
            begin
               if T in Navigate.Object'Class then
                  declare
                     Cgp : constant Navigate.Object'Class :=
                             Navigate.Object'Class (T);
                     pragma Workaround;
                     --  Cgp should be of Complex_Goto_Pose'Class type, but
                     --  this makes gnat puke on runtime so instead we use the
                     --  precise class we know we are using.
                  begin
                     Result.Set_Task
                       (Ag.Get_Name,
                        Cgp.To_Goto_Pose);
                     --  Log ("Simplifying task for " & Ag.Get_Name, Always);
                  end;
               elsif T in Tasks.Goto_Pose.Object then
                  Result.Set_Task (Ag.Get_Name, T);
                  Log ("Keeping task for " & Ag.Get_Name, Always);
               else
                  Log ("Agent " & Ag.Get_Name & " has a " &
                       External_Tag (T'Tag) & " goal (??)", Error);
                  raise Program_Error with "Task is not of Goto_Pose class";
               end if;
            end;
         end if;
      end Check;
   begin
      Result.Set_Agents (Ass.Get_Agents_Without_Tasks);

      Ass.Get_Agents.Iterate (Check'Access);

      return Result;
   end Apply_To_Simple_Goal_Transform;

   ----------------------------------
   -- Apply_Complex_Goal_Transform --
   ----------------------------------

   function Apply_To_Complex_Goal_Transform
     (Ass  : Sancta.Assignment.Object;
      M    : Map.Smart.Object)
      return Sancta.Assignment.Object
   is
      Result : Sancta.Assignment.Object;

      procedure Check (I : Ac.Lists.Cursor) is
         Ag : constant Agent.Object'Class := Ac.Lists.Element (I);
      begin
         if Ag.Has_Tasks then
            declare
               T : constant Tasks.Object'Class := Ag.Get_First_Task;
            begin
               if T in Tasks.Goto_Pose.Object then
                  declare
                     Gp : constant Tasks.Goto_Pose.Object'Class :=
                             Tasks.Goto_Pose.Object'Class (T);
                  begin
                     Result.Set_Task
                       (Ag.Get_Name,
                        Navigate.Create
                          (Gp,
                           Located_Agent.Object'Class (Ag).Get_Pose,
                           M,
                           Copy_Id => True));
                  exception
                     when E : Constraint_Error =>
                        Log ("Warning: Route finding failed for " & Ag.Get_Name,
                             Warning, Log_Section);
                        Log ("Exception: " & Report (E), Warning, Log_Section);
                        Result.Set_Task
                          (Ag.Get_Name,
                           Sancta.Tasks.Goto_Pose.Create
                             (Located_Agent.Object'Class (Ag).Get_Pose));
                  end;
               else
                  --  Keep task
                  Result.Set_Task (Ag.Get_Name, T);
               end if;
            end;
         end if;
      end Check;
   begin
      Result.Set_Agents (Ass.Get_Agents_Without_Tasks);

      Ass.Get_Agents.Iterate (Check'Access);

      return Result;
   end Apply_To_Complex_Goal_Transform;

   -----------
   -- Avoid --
   -----------

   function Avoid (M  : Sancta.Map.Bitmap.Object;
                   Cs : Float;
                   A  : Robot.Object) return Sancta.Types.Pose
   is
      use Types; use Operations;
      use Map.Bitmap;

      Adjust : constant Float := 0.8;

      function Is_Close (P1, P2 : Types.Pose; Adjust : Float) return Boolean is
      begin
         return
         abs Float (P1.X - P2.X) < Cs * Adjust and then
         abs Float (P1.Y - P2.Y) < Cs * Adjust;
      end Is_Close;

      P : constant Types.Pose := A.Get_Pose;
      L : constant Bit_Location := Bit_Location (M.To_Grid (P));
      O :          Types.Pose := Types.Origin;
      Obst_Pose   : Types.Pose;
   begin
      for R in Types.Rows'(-1) .. 1 loop
         for C in Types.Columns'(-1) .. 1 loop
            Obst_Pose := M.To_Pose (M.Loc (L.X + C, L.Y + R));
            if Is_Close (P, Obst_Pose, Adjust) and then
              (R /= 0 or else C /= 0) and then
              ((not M.Within_Bounds (L.X + C, L.Y + R)) or else
                 M.Get_At (L.X + C, L.Y + R) = Obstacle)
            then
--                 Trace.Log ("Avoiding" &
--                            Abscissa'Image (L.X + C) &
--                            Ordinate'Image (L.Y + R),
--                            Trace.Always);
               --  obstacle that is too close
               O := O + (P - M.To_Pose (M.Loc (L.X + C, L.Y + R)));
--                 O := O + (P - To_Pose (Cs, (L.X + C, L.Y + R)));
               --  Again for extra pull if too close
--                 if Is_Close (P, Obst_Pose, 0.8) then
--                    O := O + (P - To_Pose (Cs, (L.X + C, L.Y + R)));
--                 end if;
--                 if Is_Close (P, Obst_Pose, 0.7) then
--                    O := O + (P - To_Pose (Cs, (L.X + C, L.Y + R)));
--                    O := O + (P - To_Pose (Cs, (L.X + C, L.Y + R)));
--                 end if;
            end if;
         end loop;
      end loop;

      return O;
   end Avoid;

   ------------------------
   -- Find_Suitable_Path --
   ------------------------

--     function Find_Suitable_Path (Map  : Sancta.Map.Bitmap.Smart.Object;
--                                  Bot  : Robot.Object;
--                                  Goal : Types.Pose)
--                                  return Goto_Pose_Bitmap_Wavefront.Object
--     is
--     begin
--        declare
--           package Navigate renames Goto_Pose_Bitmap_Wavefront;
--           use Sancta.Types.Operations;
--
--           M : Sancta.Map.Bitmap.Object renames
--             Sancta.Map.Bitmap.Object (Map.Ref.all);
--
--           Shortest_Path : constant Navigate.Object :=
--                             Navigate.Create (Goal => Goal,
--                                              From => Bot.Get_Pose,
--                                              Map  => Map);
--           Endpoint_Dist   : constant Types.Real := Distance (Goal, Bot.Get_Pose);
--           Best_Distancing : constant Types.Real :=
--                               M.Get_Distancing
--                                 (Sancta.Map.To_Vector (Shortest_Path.Get_Route));
--           Best_Path       : Navigate.Object := Shortest_Path;
--        begin
--           --  If shortest path is good enough, return it:
--           if Best_Distancing <= Endpoint_Dist + Types.Real (M.Get_Cell_Size) then
--              return Shortest_Path;
--           end if;
--
--           Log ("Shortest Path for" & Bot.Get_Name & " is BAD. Lookin' others",
--                Always);
--
--           --  Try alternate route...
--
--           declare
--              use type Sancta.Map.Path;
--              Alt_Path : constant Sancta.Map.Path := Navigate.Get_Alternate_Path
--                (M,
--                 Bot.Get_Pose,
--                 Goal,
--                 Shortest_Path.Get_Route);
--           begin
--              --  Empty answer means no alternate (no obstacles to avoid)
--              if Alt_Path = Sancta.Map.Location_Lists.Empty_List then
--                 return Shortest_Path;
--              end if;
--
--              if M.Get_Distancing
--                (Sancta.Map.To_Vector (Alt_Path)) < Best_Distancing then
--                 return Navigate.Create (Path => Alt_Path,
--                                         Goal => Goal,
--                                         From => Bot.Get_Pose,
--                                         Map  => Map);
--              end if;
--           end;
--
--           return Best_Path;
--        exception
--           when E : others =>
--              Log ("Find_Suitable_Path (inner): " & Report (E), Warning);
--              return Shortest_Path;
--        end;
--     exception
--        when E : Constraint_Error =>
--           Log ("Find_Suitable_Path (outer): " & Report (E), Warning);
--           raise;
--     end Find_Suitable_Path;

   ----------------------
   -- Find_Nearer_Path --
   ----------------------

--     function Find_Nearer_Path (Map  : Sancta.Map.Bitmap.Smart.Object;
--                                Bot  : Robot.Object;
--                                Goal : Types.Pose;
--                                Near : Types.Pose)
--                                return Sancta.Tasks.Goto_Pose_Bitmap_Wavefront.Object
--     is
--     begin
--        declare
--           package Navigate renames Goto_Pose_Bitmap_Wavefront;
--           use type Sancta.Map.Path;
--           use Sancta.Types.Operations;
--
--           M : Sancta.Map.Bitmap.Object renames
--             Sancta.Map.Bitmap.Object (Map.Ref.all);
--
--           Shortest_Path : constant Navigate.Object :=
--                             Navigate.Create (Goal => Goal,
--                                              From => Bot.Get_Pose,
--                                              Map  => Map);
--
--           Shortest_Path_Dist : constant Types.Real :=
--                                  M.Get_Min_Distance
--                                    (Near, Shortest_Path.Get_Route);
--
--           --  Alternate route
--           Alt_Path : constant Sancta.Map.Path := Navigate.Get_Alternate_Path
--             (M,
--              Bot.Get_Pose,
--              Goal,
--              Shortest_Path.Get_Route);
--        begin
--              --  Empty answer means no alternate (no obstacles to avoid)
--              if Alt_Path = Sancta.Map.Location_Lists.Empty_List then
--                 return Shortest_Path;
--              end if;
--
--           if M.Get_Min_Distance (Near, Alt_Path) < Shortest_Path_Dist then
--              return Navigate.Create (Path => Alt_Path,
--                                      Goal => Goal,
--                                      From => Bot.Get_Pose,
--                                      Map  => Map);
--              Log ("Using NEARER path for bot" & Bot.Get_Name, Informative);
--           else
--              return Shortest_Path;
--           end if;
--        exception
--           when E : others =>
--              Log ("Find_Nearer_Path (inner): " & Report (E), Warning);
--              return Shortest_Path;
--        end;
--     exception
--        when E : Constraint_Error =>
--           Log ("Find_Nearer_Path (outer): " & Report (E), Warning);
--           raise;
--     end Find_Nearer_Path;

   ----------------------
   -- Wait_For_U_Turns --
   ----------------------

--     procedure Wait_For_U_Turns (Team : in out Assignment.Object;
--                                 Dirs :        Navdir_Array)
--     is
--        Nav_Dir_Delta : constant array (Nav_Dirs) of Integer := (-1, 1);
--
--        Result  : Assignment.Object;
--        Changes : Boolean := False;
--
--        use Ac.Lists;
--        procedure Check (I : Cursor) is
--           Bot : Robot.Object renames Robot.Object (Element (I));
--        begin
--           if
--             Bot.Has_Tasks and then
--             Bot.Get_First_Task in Navigate.Object and then
--             Map.Bitmap.Is_U_Turn (Navigate.Object (Bot.Get_First_Task).Get_Route)
--           then
--              Changes := True;
--              Result.Set_Task (Bot.Get_Name, Bot.Get_First_Task);
--              declare
--                 Bot_Idx   : constant Natural := Natural'Value (Bot.Get_Name);
--                 Other_Idx : constant Natural :=
--                               Natural'Value (Bot.Get_Name) +
--                               Nav_Dir_Delta (Dirs (Bot_Idx));
--                 Other     : Robot.Object renames Robot.Object
--                   (Team.Get_Agent (Other_Idx'Img));
--              begin
--                 Log ("[U-turning]" & Bot.Get_Name & " " &
--                      Dirs (Bot_Idx)'Img & Other.Get_Name, Informative);
--                 if Other_Idx in Dirs'Range then
--                    Result.Set_Task
--                      (Other.Get_Name,
--                       Find_Suitable_Path
--                         (The_Map,
--                          Other,
--                          Bot.Get_Pose));
--                 end if;
--              end;
--           end if;
--        end Check;
--     begin
--        Result.Set_Agents (Team.Get_Agents_Without_Tasks);
--
--        Team.Get_Agents.Iterate (Check'Access);
--
--        if Changes then
--           Team := Result;
--        end if;
--     end Wait_For_U_Turns;

end Sancta.Ctree.Utils;

