with Agpl.Containers.Bags;
--  with Agpl.Containers.Integer_Sets;
with Agpl.Random;
with Sancta.Ctree.Utils;
with Sancta.Tasks.Utils;

package body Sancta.Ctree.Parallel is

   use Sancta.Map.Paths;

   use type Ada.Containers.Count_Type;
   use type Sancta.Costs;
   use type Sancta.Map.Location'Class;

   subtype Utils_Node is Utils.Node_Data;

   -----------------
   -- Bots_Needed --
   -----------------

   function Bots_Needed (Depth : Sancta.Costs;
                         Limit : Network_Range) return Positive is
   begin
      return Positive (Float'Ceiling (Float (Depth) / Limit));
   end Bots_Needed;

   -------------------
   -- Relays_Needed --
   -------------------

   function Relays_Needed (Depth : Sancta.Costs;
                           Limit : Network_Range) return Natural is
   begin
      return Natural (Float'Floor (Float (Depth) / Limit));
   end Relays_Needed;

   ----------------
   -- Build_Tree --
   ----------------

   procedure Build_Tree
     (Tree  : out Path_Trees.Tree;
      Nav   :     Tree_Navigator.Object'Class;
      Tasks :     Tc.Lists.List)
   is
      procedure Build_Tree (I : Path_Trees.Cursor) is
      begin
         I.Iterate_Children (Build_Tree'Access);

         declare
            Old_Data : constant    Utils.Node_Data := Utils_Node (I.Element);
            New_Data :          Parallel.Node_Data :=
                         (Pos            => I,
                          Loc            => Old_Data.Loc,
                          Blocked        => False,
                          Tasks_Here     => Old_Data.Tasks,
                          Tasks_Below    => <>,
                          Cost_From_Root => Old_Data.Cost_From_Root,
                          Lazy_Concurrent => 0);

            procedure Append_Tasks_Below (I : Path_Trees.Cursor) is
               use Sancta.Tasks.Utils;
            begin
               --  Note that internal tasks are no relevant, so omitted here
               if I.Is_Leaf then
                  Concatenate (New_Data.Tasks_Below,
                               Parallel.Node_Data (I.Query.all).Tasks_Here);
               end if;

               Concatenate (New_Data.Tasks_Below,
                            Parallel.Node_Data (I.Query.all).Tasks_Below);
            end Append_Tasks_Below;
         begin
            I.Iterate_Children (Append_Tasks_Below'Access);
            I.Include (New_Data);
         end;
      end Build_Tree;
   begin
      Utils.Build_Tree (Tree, Nav, Tasks);
      Utils.Build_Costs (Path_Trees.Cursor (Tree.Root));

      Build_Tree (Path_Trees.Cursor (Tree.Root));
   end Build_Tree;

   --------------
   -- Fix_Tree --
   --------------

   procedure Fix_Tree (T : Path_Trees.Tree) is
   --  Needed because the nodes self-reference via cursors, so
   --  on assignment these cursors are off-tree in the old tree.

      procedure Fix_Node (I : Path_Trees.Cursor) is
      begin
         Node_Data (I.Update.all).Pos := I;
         I.Iterate_Children (Fix_Node'Access);
      end Fix_Node;

   begin
      Fix_Node (Path_Trees.Cursor (T.Root));
   end Fix_Tree;

   Id_Hash   : Tc.Lists.List;
   Id_Cost   : Id_Cost_Maps.Map;
   --  Profiler determined this to be a speed up.

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Splitter  :     access
        function (Conf  : Split_Context;
                  Nodes : Node_Data_Array;
                  Nav   : Tree_Navigator.Object'Class;
                  Costs : Id_Cost_Maps.Map)
                  return  Team_Array;
      Map       :     Sancta.Map.Object'Class;
      Navigator :     Tree_Navigator.Object'Class;
      Tree      :     Path_Trees.Tree;
      --  Nodes of Parallel.Node_Data
      Bots      :     Positive;
      Limit     :     Network_Range;
      Criterion :     Sancta.Criteria.Enum_Criteria;
      Stop      :     Sancta.Costs; -- Threshold for aborting
      Cost      : out Sancta.Costs)
   is
      All_Bots  : constant Positive      := Bots;

      All_Tasks : constant Tc.Lists.List := Navigator.Get_Tasks;
      Num_Tasks : constant Positive      := Positive (Navigator.Get_Tasks.Length);

      ------------------
      -- Build_Tables --
      ------------------

      procedure Build_Tables is
         procedure Build_Tables (Pos : Path_Trees.Cursor) is
            procedure Add (I : Tc.Lists.Cursor) is
            begin
               Id_Cost.Insert (Tc.Lists.Element (I).Get_Id,
                               Node_Data (Pos.Element).Cost_From_Root);
            end Add;
         begin
            Node_Data (Pos.Element).Tasks_Here.Iterate (Add'Access);
            Pos.Iterate_Children (Build_Tables'Access);
         end Build_Tables;
      begin
         Build_Tables (Path_Trees.Cursor (Tree.Root));
      end Build_Tables;

      Sum : Sancta.Costs := 0.0;
      Ave : Sancta.Costs := 0.0;

      --------------
      -- Evaluate --
      --------------

      procedure Evaluate (Branch_Id:        Natural;
                          From     :        Path_Trees.Cursor'Class;
                          Relays_In_Parent : Natural;
                          Bots     :        Positive;
                          Max      : in out Sancta.Costs)
      is
         Node      : Node_Data renames Node_Data (From.Update.all);
         Relays    : constant Natural :=
                       Relays_Needed (Node.Cost_From_Root, Limit);

         Step      : Sancta.Costs;

         function Target_Cost return Sancta.Costs is
            use Sancta.Criteria;
         begin
            case Criterion is
               when MinSum => return Sum;
               when MinMax => return Max;
               when MinAve => return Ave;
               when others => raise Program_Error with "Unimplemented";
            end case;
         end Target_Cost;

         procedure Report (Prefix : String) is
         begin
            if not Do_Report then
               return;
            end if;

            Log (Branch_Id'Img & "#: " &
                 Prefix & " " &
                 "Depth"  & Node.Cost_From_Root'Img & "; " &
                 "Rels"   & Relays'Img & "; " &
                 "Mobi"   & Natural'Image
                     (Bots - Relays_Needed (Node.Cost_From_Root, Limit)) & "; " &
                 "Tblw"   & Node.Tasks_Below.Length'Img & "; " &
                 "MinN"   & Node.Min_Bots_Needed (Limit, Id_Cost)'Img & "; " &
                 "MaxN"   & Node.Max_Bots_Needed (Limit, Id_Cost)'Img & "; " &
                 "Cost"   & Target_Cost'Img,
                 Debug, Log_Section);
         end Report;

         ----------------
         -- Accomplish --
         ----------------

         procedure Accomplish (I : Tc.Lists.Cursor) is
            use Sancta.Tasks.Utils;
            Pos : Path_Trees.Cursor := Path_Trees.Cursor (From.Parent);
            Key : constant Sancta.Tasks.Task_Id := Tc.Lists.Element (I).Get_Id;
         begin
            Report ("TARGET");
            Ave := Ave + Max;

            Node.Lazy_Concurrent := 0;

            if From.Is_Leaf and then not From.Is_Root then
               --  If it were not leaf, the tasks wouldnt be in the
               --  TasK_Below lists to begin with...
               loop
                  Delete (Node_Data (Pos.Update.all).Tasks_Below, Key);
                  Node_Data (Pos.Update.all).Lazy_Concurrent := 0;
                  exit when Pos.Is_Root;
                  Pos := Pos.Parent;
               end loop;
            end if;
         end Accomplish;

         ---------------------
         -- Make_Candidates --
         ---------------------

         function Make_Candidates return Node_Data_Array is
            Candidates : Node_Data_Array (1 .. From.Child_Count);
            Last       : Natural := Candidates'First - 1;
            procedure Append (I : Path_Trees.Cursor) is
               Data : Node_Data renames Node_Data (I.Query.all);
            begin
               if not Data.Blocked and then
                 (Data.Tasks_Here.Length + Data.Tasks_Below.Length > 0)
               then
                  Last := Last + 1;
                  Candidates (Last) := Node_Data (I.Element);
               end if;
            end Append;
         begin
            From.Iterate_Children (Append'Access);
--              Log ("CANDIDATES" & Last'Img, Debug, Log_Section);
            return Candidates (Candidates'First .. Last);
         end Make_Candidates;

         ---------------
         -- Do_Splits --
         ---------------

         procedure Do_Splits is
            Available  : Natural := Bots;

            Team_Times : Timed_Team_Sets.Set;
            --  These are the teams that got split from here.
            --  We merge them back in finishing time order, and re-split
            --    after each one ends.

            Forced : Boolean;
         begin
            while not (Node.Tasks_Below.Is_Empty and then Team_Times.Is_Empty)
            loop
               Forced := False;

               --  Execute new splits, if any:
               declare
                  use Sancta.Criteria;
                  Candidates : constant Node_Data_Array := Make_Candidates;
               begin
                  --  Check that we have a full branch at our disposal and use:
                  if Do_Force and then Criterion /= Minave then
                     for I in Candidates'Range loop
                        if Candidates (I).Max_Bots_Needed (Limit, Id_Cost) =
                           All_Bots
                        then
                           Forced := True;
                           Report ("FORCE");
                           Evaluate (Branch_Id,
                                     Candidates (I).Pos,
                                     Relays,
                                     All_Bots,
                                     Max);
                        end if;
                     end loop;
                  end if;

                  if not Forced then
                     if Candidates'Length > 0 then
                        if From.Child_Count > 1 then
                           Report ("SPLIT" & Available'Img);
                        end if;

                        declare
                           New_Teams  : constant Team_Array :=
                                          Splitter ((Node.Cost_From_Root,
                                            Limit,
                                            Available),
                                            Candidates,
                                            Navigator,
                                            Id_Cost);
                        begin
                           --  Prevent anyone else going down this path:
                           for I in New_Teams'Range loop
                              Node_Data
                                (New_Teams (I).Next_Node.Update.all).Blocked := True;
                              Available := Available - (New_Teams (I).Bots - Relays);
                           end loop;

                           --  Actually compute the splitted teams outcome
                           for I in New_Teams'Range loop
                              declare
                                 use type Path_Trees.Cursor;
                                 Mobile_Bots : constant Positive :=
                                                 New_Teams (I).Bots - Relays;
                                 Local_Max   : Sancta.Costs := Max;
                              begin
                                 Evaluate (New_Teams (I).Id,
                                           New_Teams (I).Next_Node,
                                           Relays,
                                           New_Teams (I).Bots,
                                           Local_Max);
                                 Team_Times.Insert
                                   (Timed_Teams'(Local_Max,
                                    Mobile_Bots,
                                    New_Teams (I)));
                              end;
                           end loop;
                        end;
                     else
                        Report ("WAIT");
                     end if;
                  end if;
               end;

               --  Merge back first returned:
               if not Forced then
                  if not Team_Times.Is_Empty then
                     if Team_Times.Length > 1 then
                        Report ("TEAMS" & Team_Times.Length'Img);
                     end if;
                     declare
                        Returned : constant Timed_Teams := Team_Times.First_Element;
                     begin
                        Team_Times.Delete_First;

                        Node_Data (Returned.Team.Next_Node.Update.all).Blocked := False;
                        Available := Available + Returned.Mobile_Bots;
                        Max       :=             Returned.Time_Back;
                        if From.Child_Count > 1 then
                           Report ("MERGE" & Returned.Team.Id'Img & "#");
                           Log ("Returned" & Returned.Time_Back'Img & "; " &
                                "Mobile" & Returned.Mobile_Bots'Img & "; " &
                                "Available" & Available'Img,
                                Debug, Log_Section);
                        end if;
                     end;
                  else
                     Report ("RETREAT");
                     --                    raise Program_Error with "NO MERGERS";
                     exit;
                  end if;
               else
                  Report ("RETRY");
               end if;
            end loop;
         end Do_Splits;

         Retreat : Boolean := False;

      begin
         --  Cost of arriving here.
         if not From.Is_Root then
            Step := Map.Get_Cost_Between
              (Node_Data (From.Parent.Query.all).Loc.Ref.all,
               Node.Loc.Get);

            Sum := Sum + Step * (Bots - Relays_Needed (Node.Cost_From_Root, Limit));
            Max := Max + Step;
         end if;
         if Detailed or Relays /= Relays_In_Parent then
            Report ("DOWN");
         end if;

         --  Complete tasks in current node.
         if not Node.Tasks_Here.Is_Empty then
            Node.Tasks_Here.Iterate (Accomplish'Access);
            Node.Tasks_Here.Clear;

            --  Check for early return for the case of Split_When_Any_Reachable
            if not (From.Child_Count = 0) and then
              Bots < Node.Min_Bots_Needed (Limit, Id_Cost)
            then
               Report ("BACK");
               Retreat := True;
            end if;
         end if;

         if Target_Cost > Stop then
            raise Cost_Exceeded;
         end if;

         --  LEAF, EXHAUST, CONTINUE, or SPLIT
         if not Retreat then
            if From.Child_Count = 0 then
               Report ("LEAF");
            elsif Bots = Relays_Needed (Node.Cost_From_Root, Limit) then
               Report ("EXHAUSTED");
            elsif From.Child_Count = 1 then
               Evaluate (Branch_Id, From.First_Child, Relays, Bots, Max);
            else
               Do_Splits;
            end if;
         end if;

         --  Cost of moving up
         if not From.Is_Root then
            Sum := Sum + Step * (Bots - Relays_Needed (Node.Cost_From_Root, Limit));
            Max := Max + Step;
         end if;

         if Target_Cost > Stop then
            raise Cost_Exceeded;
         end if;

         if Detailed or Relays /= Relays_In_Parent then
            Report ("UP  ");
         end if;
      exception
         when others =>
            Report ("EXCEPTION");
            raise;
      end Evaluate;

      use type Tc.Lists.List;

      Max : Sancta.Costs := 0.0;

   begin
      if Id_Hash /= All_Tasks then
         Id_Hash := All_Tasks;
         Build_Tables;
      end if;

      Fix_Tree (Tree);

      Log ("Optimal needed:" &
           Concurrent_Bots_Needed
             (Node_Data (Tree.Root.Query.all), Limit, Id_Cost)'Img,
           Debug, Log_Section);

      Evaluate (1000, Tree.Root, 0, Bots, Max);

      pragma Assert (Node_Data (Tree.Root.Query.all).Tasks_Here.Is_Empty);
      pragma Assert (Node_Data (Tree.Root.Query.all).Tasks_Below.Is_Empty);

      Ave := Ave / Sancta.Costs (Num_Tasks);

      declare
         use Sancta.Criteria;
      begin
         case Criterion is
            when MinSum => Cost := Sum;
            when MinMax => Cost := Max;
            when MinAve => Cost := Ave;
            when others => raise Program_Error with "Unimplemented";
         end case;
      end;
   end Evaluate;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Timed_Teams) return Boolean is
   begin
      return L.Time_Back < R.time_Back;
   end "<";

   --------------------------
   -- Split_Single_Min_Max --
   --------------------------

   function Split_Single_Min_Max
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array
   is
      pragma Unreferenced (Nav);
      Best_Index : Positive     := Nodes'Last + 1; -- Ensure failure if AWOL
      Best_Cost  : Sancta.Costs := Sancta.Infinite;
   begin
      if Nodes'Length = 1 then
         Best_Index := Nodes'First;
      else
         for I in Nodes'Range loop
            declare
               procedure Find_Closest (J : Tc.Lists.Cursor) is
               begin
                  if Costs.Element (Tc.Lists.Element (J).Get_Id) < Best_Cost then
                     Best_Cost  := Costs.Element (Tc.Lists.Element (J).Get_Id);
                     Best_Index := I;
                  end if;
               end Find_Closest;
            begin
               Nodes (I).Tasks_Here.Iterate (Find_Closest'Access);
               Nodes (I).Tasks_Below.Iterate (Find_Closest'Access);
            end;
         end loop;
      end if;

      return (1 => (True,
                    Nodes (Best_Index).Cursor,
                    Conf.Bots,
                    0));
   end Split_Single_Min_Max;

   ---------------------------
   -- Split_Single_Preorder --
   ---------------------------

   function Split_Single_Preorder
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array
   is
      pragma Unreferenced (Nav, Costs);
   begin
      return (1 => (True,
                    Nodes (Nodes'First).Cursor,
                    Conf.Bots,
                    0));
   end Split_Single_Preorder;

   -------------------
   -- Split_Ordered --
   -------------------

   Split_Id : Natural := 1001;

   function Split_Ordered
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array
   is
      pragma Unreferenced (Nav);

      use Sancta.Tasks.Utils;

      Jobs   : Tc.Lists.List :=
                 Order and
                 Node_Data
                   (Nodes (Nodes'First).Pos.Root_Cursor.Query.all).Tasks_Below;

      Teams  : Team_Array (Nodes'Range);

      Relays    : constant Natural := Relays_Needed (Conf.Depth, Conf.Limit);
      Available :          Natural := Conf.Bots;

      Used   : array (Nodes'Range) of Boolean := (others => False);
      Last   : Natural := Teams'First - 1;

      function Find_Branch (Job : Sancta.Tasks.Object'Class)
                            return     Positive
      is
      begin
         for I in Nodes'Range loop
            if not Used (I) then
               if Nodes (I).Contains (Job) then
                  return I;
               end if;
            end if;
         end loop;

         return Nodes'Last + 1;
      end Find_Branch;

      --------------------
      -- Find_Next_Task --
      --------------------

      function Find_Next_Task return Sancta.Tasks.Object'Class is
      begin
         return Jobs.First_Element;
      end Find_Next_Task;

      Max_Required : Positive := 1;
   begin
      loop
         if Teams'Length = 0 then
            Log ("SPLIT Exiting due to no branchs given", Debug, Log_Section);
            exit;
         elsif Last = Teams'Last then
            Log ("SPLIT Exiting due to branchs exhausted", Debug, Log_Section);
            exit;
         elsif Available = 0 then
            Log ("SPLIT No robots left for next branch, av" & Available'Img,
                 Debug, Log_Section);
            exit;
         end if;

         declare
            Job      : constant Sancta.Tasks.Object'Class := Find_Next_Task;
            I        : constant Positive := Find_Branch (Job);
            Mobile   :          Positive;
            Required :          Positive;
         begin
            if I not in Nodes'Range then
               Log ("SPLIT Exiting due to no candidate tasks in remaining branches",
                    Debug, Log_Section);
               exit;
            end if;

            Used (I) := True;

            case Split_Policy is
               when Some_Task_Is_Reachable =>
                  Required :=
                    Bots_Needed (Costs.Element (Job.Get_Id), Conf.Limit);
               when Farthest_Task_Is_Reachable =>
                  Required := Nodes (I).Max_Bots_Needed (Conf.Limit, Costs);
               when All_Tasks_Are_Reachable =>
                  Required :=
                    Natural'Max
                      (Nodes (I).Max_Bots_Needed (Conf.Limit, Costs),
                       Natural'Min
                         (Conf.Bots,
                          Nodes (I).Concurrent_Bots_Needed (Conf.Limit, Costs)));
               when Farthest_Tasks_Is_Reachable_And_Go_For_Spares =>
                  if Available >=
                    Nodes (I).Max_Bots_Needed (Conf.Limit, Costs)
                  then
                     Required := Nodes (I).Max_Bots_Needed (Conf.Limit, Costs);
                  else
                     Required :=
                       Bots_Needed (Costs.Element (Job.Get_Id), Conf.Limit);
                     if Required >= Max_Required then
                        Log ("SPLIT Exiting due to required >= max_required",
                             Debug, Log_Section);
                        exit;
                     end if;
                  end if;

                  if Required > Available or else
                    Required <= Relays
                  then
                     Log ("SPLIT Exiting due to improper required",
                          Debug, Log_Section);
                     exit;
                  end if;

                  Max_Required := Natural'Max (Max_Required, Required);
            end case;

            begin
               Jobs   := Jobs - Nodes (I).Reachable_Targets (Required,
                                                             Conf.Limit,
                                                             Costs);
               Mobile := Required - Relays;
            exception
               when others =>
                  null;
                  raise;
            end;

            if Required <= Available then

               Last := Last + 1;
               Split_Id := Split_Id + 1;

               Teams (Last) := (True,
                                Nodes (I).Cursor,
                                Required,
                                Split_Id);
               Available := Available - Mobile;

               Log ("SPLIT" & Teams (Last).Id'Img & "# " &
                    "branch" & Last'Img & "; " &
                    "rels" & Relays'Img & "; " &
                    "mobi" & Mobile'Img & "; " &
                    "availafter" & Available'Img,
                    Debug, Log_Section);
            else
               Log ("SPLIT No robots left for next branch, av" &
                    Available'Img & "; req" & Required'Img,
                    Debug, Log_Section);
               exit;
            end if;
         end;
      end loop;

      return Teams (Teams'First .. Last);
   end Split_Ordered;

   ----------------
   -- Split_Fill --
   ----------------

   procedure Split_Fill
     (Conf      : Split_Context;
      Available : Positive;
      Max_Bots  : Positive;
      Nodes     : Node_Data_Array;
      Used      : in out Boolean_Array;
      Nav       : Tree_Navigator.Object'Class;
      Costs     : Id_Cost_Maps.Map;
      Teams     : in out Team_Array;
      Last      : in out Natural)
   is
      pragma Unreferenced (Nav);

      Relays   : constant Natural := Relays_Needed (Conf.Depth, Conf.Limit);
      Assigned :          Natural := 0;
      Found    :          Boolean := False;
      Checked  :          Natural := 0;
   begin
      while Assigned < Available loop
         Found := False;

         for I in Nodes'Range loop
            declare
               Required : Positive;
            begin
               if not Used (I) then
                  Checked  := Checked + 1;
                  Required := Nodes (I).Min_Bots_Needed (Conf.Limit, Costs);
                  if Required <= Available and then
                    Required < Max_Bots and then
                    Required > Relays
                  then
                     Split_Id  := Split_Id + 1;
                     Log ("FILL  filling in" & Split_Id'Img & "#, req" & Required'Img,
                          Debug, Log_Section);
                     Assigned  := Assigned + (Required - Relays);
                     Last      := Last     + 1;
                     Used (I)  := True;
                     Found     := True;
                     Teams (Last) := (True,
                                      Nodes (I).Cursor,
                                      Required,
                                      Split_Id);
                  end if;
               end if;
            end;
         end loop;

         if not Found then
            Log ("FILL  no candidate found, max" & Max_Bots'Img &
                 " checked" & Checked'Img,
                 Debug, Log_Section);
            exit;
         end if;
      end loop;
   end Split_Fill;

   ---------------------
   -- Split_Heuristic --
   ---------------------

   function Split_Heuristic
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array
   is
      Teams  : Team_Array (Nodes'Range);
      Last   : Natural := Teams'First - 1;

      Used   : Boolean_Array (Nodes'Range) := (others => False);

      Relays      : constant Natural := Relays_Needed (Conf.Depth, Conf.Limit);
      Available   :          Natural := Conf.Bots;
      Max_Allowed :          Natural := 0;

      -------------------
      -- Find_Farthest --
      -------------------

      function Find_Farthest return Natural is
         Best_Req   : Natural  := 0;
         Best_Idx   : Positive := Nodes'Last + 1;
         Req        : Natural;
      begin
         for I in Nodes'Range loop
            if not Used (I) then
               Req := Nodes (I).Max_Bots_Needed (Conf.Limit, Costs);
               if Req > Best_Req then
                  Best_Req := Req;
                  Best_Idx := I;
               end if;
            end if;
         end loop;

         return Best_Idx;
      end Find_Farthest;

      ------------------
      -- Find_Closest --
      ------------------

      function Find_Closest return Natural is
         Best_Req   : Natural  := Natural'Last;
         Best_Idx   : Positive := Nodes'Last + 1;
         Req        : Natural;
      begin
         for I in Nodes'Range loop
            if not Used (I) then
               Req := Nodes (I).Max_Bots_Needed (Conf.Limit, Costs);
               if Req < Best_Req then
                  Best_Req := Req;
                  Best_Idx := I;
               end if;
            end if;
         end loop;

         return Best_Idx;
      end Find_Closest;

      Index    : Natural;
      Required : Positive;

      Some_Found : Boolean := False;

   begin
      loop
         if Last = Teams'Last then
            Log ("SPLIT Exiting due to branchs exhausted", Debug, Log_Section);
            exit;
         end if;

         case Split_Policy is
            when Heu_Farthest_First =>
               Index := Find_Farthest;
            when Heu_Closest_First =>
               Index := Find_Closest;
         end case;

         if Index not in Nodes'Range then
            Log ("SPLIT Exiting due to no candidate branches",
                 Debug, Log_Section);
            exit;
         end if;

         Used (Index) := True;

         case Amount_Policy is
            when Sequential_Needed =>
               Required     := Nodes (Index).Max_Bots_Needed (Conf.Limit, Costs);
               Max_Allowed  := Natural'Max (Max_Allowed, Required);
            when Concurrent_Needed =>
               Required     := Natural'Max
                 (Nodes (Index).Max_Bots_Needed (Conf.Limit, Costs),
                  Natural'Min
                    (Conf.Bots,
                     Nodes (Index).Concurrent_Bots_Needed (Conf.Limit, Costs)));
               Max_Allowed  := Natural'Max
                 (Max_Allowed,
                  Natural'Min
                    (Required,
                     Nodes (Index).Max_Bots_Needed (Conf.Limit, Costs)));
         end case;

         if Required <= Relays then
            Log ("SPLIT Exiting due to required <= relays",
                 Debug, Log_Section);
            exit;
         elsif Required > Available then
            Log ("SPLIT No robots left for next branch, av" &
                 Available'Img & "; req" & Required'Img,
                 Debug, Log_Section);
            exit;
         end if;

         Last     := Last + 1;
         Split_Id := Split_Id + 1;

         Teams (Last) := (True,
                          Nodes (Index).Cursor,
                          Required,
                          Split_Id);

         Available  := Available - (Required - Relays);
         Some_Found := True;

         Log ("SPLIT" & Teams (Last).Id'Img & "# " &
              "branch" & Last'Img & "; " &
              "rels" & Relays'Img & "; " &
              "mobi" & Natural'Image (Required - Relays) & "; " &
              "availafter" & Available'Img,
              Debug, Log_Section);
      end loop;

      --  Fill in partial shadowed branches:
      if Available > Relays then
         if Some_Found then
            Split_Fill
              (Conf, Available, Max_Allowed, Nodes, Used, Nav, Costs, Teams, Last);
         else
            --  We are in a partial branch, thus allowed are as many as we have:
            Split_Fill
              (Conf, Available, Conf.Bots, Nodes, Used, Nav, Costs, Teams, Last);
         end if;
      end if;

      return Teams (Teams'First .. Last);
   end Split_Heuristic;

   ----------------------
   -- Split_Stochastic --
   ----------------------

   function Split_Stochastic
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array
   is
      pragma Unreferenced (Nav);

      Teams  : Team_Array (Nodes'Range);
      Last   : Natural := Teams'First - 1;

      Available   :          Natural := Conf.Bots;
      Relays      : constant Natural := Relays_Needed (Conf.Depth, Conf.Limit);

      package Node_Bags is new Agpl.Containers.Bags (Node_Data, Integer);
      package Int_Bags  is new Agpl.Containers.Bags (Positive,  Integer);

      Candidates : Node_Bags.Object (First => 1);

      procedure Add_Split (Node : Node_Data; Bots : Positive) is
      begin
         Last     := Last + 1;
         Split_Id := Split_Id + 1;

         Teams (Last) := (True,
                          Node.Cursor,
                          Bots,
                          Split_Id);

         Available  := Available - (Bots - Relays);

         if Do_Report then
            Log ("SPLIT" & Teams (Last).Id'Img & "# " &
                 "branch" & Last'Img & "; " &
                 "rels" & Relays'Img & "; " &
                 "mobi" & Natural'Image (Bots - Relays) & "; " &
                 "availafter" & Available'Img,
                 Debug, Log_Section);
         end if;
      end Add_Split;

      procedure Generate_Full_Candidates is
      begin
         for I in Nodes'Range loop
            if Nodes (I).Max_Bots_Needed (Conf.Limit, Costs) <= Conf.Bots then
               Candidates.Append (Nodes (I));
            end if;
         end loop;
         if Do_Report and then Detailed then
            Log ("STOC full_candidates.length =" & Candidates.Length'Img,
                 Debug, Log_Section);
         end if;
      end Generate_Full_Candidates;

      procedure Generate_Partial_Candidates is
      begin
         for I in Nodes'Range loop
            if not Node_Data (Nodes (I).Pos.Query.all).Blocked and then
              Nodes (I).Min_Bots_Needed (Conf.Limit, Costs) <= Available
            then
               Candidates.Append (Nodes (I));
            end if;
         end loop;
      end Generate_Partial_Candidates;

      procedure Extract_Full_Branch is
         Index : constant Positive :=
                   Agpl.Random.Get_Integer (Candidates.First,
                                            Candidates.Last);
         Node    : Node_Data renames Candidates.Vector (Index);
         Amounts : Int_Bags.Object (First => 1);

         Max     : constant Positive := Node.Max_Bots_Needed (Conf.Limit, Costs);
      begin
         for I in Max .. Positive'Max (Max, Available) loop
            Amounts.Append (I);
         end loop;

         pragma Assert (not Amounts.Is_Empty);

         Add_Split
           (Candidates.Vector (Index),
            Amounts.Vector
              (Agpl.Random.Get_Integer (Amounts.First, Amounts.Last)));

         Node_Data (Candidates.Vector (Index).Pos.Update.all).Blocked := True;
      end Extract_Full_Branch;

      procedure Extract_Any_Branch is
         Index : constant Positive :=
                   Agpl.Random.Get_Integer (Candidates.First,
                                            Candidates.Last);
         Node    : Node_Data renames Candidates.Vector (Index);
         Amounts : Int_Bags.Object (First => 1);

         Min     : constant Positive := Node.Min_Bots_Needed (Conf.Limit, Costs);
      begin
         for I in Min .. Available loop
            Amounts.Append (I);
         end loop;

         pragma Assert (not Amounts.Is_Empty);

         Add_Split
           (Candidates.Vector (Index),
            Amounts.Vector
              (Agpl.Random.Get_Integer (Amounts.First, Amounts.Last)));
      end Extract_Any_Branch;

      procedure Remove_Invalid is
      begin
         for I in reverse Candidates.First .. Candidates.Last loop
            if Candidates.Vector (I).Min_Bots_Needed (Conf.Limit, Costs) >
              Available
            then
               Candidates.Delete (I);
            end if;
         end loop;
      end Remove_Invalid;

   begin

      if Split_Policy = Full_Branch_First then

         Generate_Full_Candidates;

         if not Candidates.Is_Empty then
            if Do_Report and then Detailed then
               Log ("STOC extracting full branch", Debug, Log_Section);
            end if;
            Extract_Full_Branch;
            Remove_Invalid;
         end if;

      end if;

      Generate_Partial_Candidates;

      loop
         if Candidates.Is_Empty then
            Log ("STOC no any candidates", Debug, Log_Section);
            exit;
         elsif Last >= Teams'First and then Agpl.Random.Uniform < 0.5 then
            Log ("STOC probabilistic bailout", Debug, Log_Section);
            exit;
         elsif Last = Teams'Last then
            Log ("STOC teams exhausted bailout", Debug, Log_Section);
            exit;
         end if;

         if Detailed then
            Log ("STOC extracting any branch", Debug, Log_Section);
         end if;
         Extract_Any_Branch;
         Remove_Invalid;
      end loop;

      return Teams (Teams'First .. Last);
   end Split_Stochastic;

   ------------
   -- Cursor --
   ------------

   function Cursor (This : Node_Data) return Path_Trees.Cursor is
   begin
      return This.Pos;
   end Cursor;

   --------------
   -- Contains --
   --------------

   function Contains (This : Node_Data;
                      Job  : Sancta.Tasks.Object'Class) return Boolean is
   begin
      return This.Tasks_Here.Contains (Job) or else This.Tasks_Below.Contains (Job);
   end Contains;

   ---------------------
   -- Min_Bots_Needed --
   ---------------------

   function Min_Bots_Needed (This  : Node_Data;
                             Limit : Network_Range;
                             Costs : Id_Cost_Maps.Map) return Natural
   is
      Min : Natural := Natural'Last;
      procedure Check_Needed (I : Tc.Lists.Cursor) is
      begin
         Min :=
           Natural'Min
             (Min,
              Bots_Needed (Costs.Element (Tc.Lists.Element (I).Get_Id), Limit));
      end Check_Needed;
   begin
      if This.Pos.Is_Leaf then
         This.Tasks_Here.Iterate  (Check_Needed'Access);
      end if;
      This.Tasks_Below.Iterate (Check_Needed'Access);

      return Min;
   end Min_Bots_Needed;

   ---------------------
   -- Max_Bots_Needed --
   ---------------------

   function Max_Bots_Needed (This  : Node_Data;
                             Limit : Network_Range;
                             Costs : Id_Cost_Maps.Map) return Natural
   is
      Max : Natural := 0;
      procedure Check_Needed (I : Tc.Lists.Cursor) is
      begin
         Max :=
           Natural'Max
             (Max,
              Bots_Needed (Costs.Element (Tc.Lists.Element (I).Get_Id), Limit));
      end Check_Needed;
   begin
      if This.Pos.Is_Leaf then
         This.Tasks_Here.Iterate  (Check_Needed'Access);
      end if;
      This.Tasks_Below.Iterate (Check_Needed'Access);

      return Max;
   end Max_Bots_Needed;

   ----------------------------
   -- Concurrent_Bots_Needed --
   ----------------------------

   function Concurrent_Bots_Needed (This  : Node_Data;
                                    Limit : Network_Range;
                                    Costs : Id_Cost_Maps.Map) return Natural
   is
      pragma Unreferenced (Costs);
   begin
      if This.Lazy_Concurrent > 0 then
         return This.Lazy_Concurrent;
      end if;

      declare
         Needed : Positive := 1 + Relays_Needed (This.Cost_From_Root, Limit);

         procedure Traversal (I : Path_Trees.Cursor) is
            Node : Node_Data renames Node_Data (I.Query.all);
         begin
            --  One extra bot needed for each branching:
            if I.Has_Children then
               Needed := Needed + I.Child_Count - 1;
            end if;

            --  One extra bot for each relay on the way down:
            if not I.Is_Root then
               Needed := Needed +
                 Relays_Needed (Node.Cost_From_Root, Limit) -
                 Relays_Needed (Node_Data (I.Parent.Query.all).Cost_From_Root, Limit);
            end if;

            I.Iterate_Children (Traversal'Access);
         end Traversal;
      begin
         Traversal (This.Pos);

         Node_Data (This.Pos.Update.all).Lazy_Concurrent := Needed;

         return Needed;
      end;
   end Concurrent_Bots_Needed;

   -----------------------
   -- Reachable_Targets --
   -----------------------

   function Reachable_Targets (This : Node_Data;
                               Bots : Natural;
                               Limit : Network_Range;
                               Costs : Id_Cost_Maps.Map) return Tc.Lists.List
   is
      Result : Tc.Lists.List;

      procedure Add_If_Reachable (I : Tc.Lists.Cursor) is
         Job : constant Sancta.Tasks.Object'Class := Tc.Lists.Element (I);
      begin
         if Bots_Needed (Costs.Element (Job.Get_Id), Limit) <= Bots then
            Result.Append (Job);
         end if;
      end Add_If_Reachable;
   begin
      This.Tasks_Here.Iterate  (Add_If_Reachable'Access);
      This.Tasks_Below.Iterate (Add_If_Reachable'Access);
      return Result;
   end Reachable_Targets;

   --------------------------
   -- Tasks_Here_And_Below --
   --------------------------

   function Tasks_Here_And_Below (This : Node_Data) return Tc.Lists.List is
      use Sancta.Tasks.Utils;
      Result : Tc.Lists.List := This.Tasks_Here;
   begin
      Concatenate (Result, This.Tasks_Below);
      return Result;
   end Tasks_Here_And_Below;

end Sancta.Ctree.Parallel;
