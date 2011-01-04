with Agpl.Strings;  use Agpl.Strings;
with Agpl.Ustrings; use Agpl.Ustrings;
with Sancta.Tasks.Utils;

package body Sancta.Ctree.Parallel.Brute is

--     function Cmin (L, R : Sancta.Costs) return Sancta.Costs
--                    renames Sancta.Costs'Min;

--     function Cmax (L, R : Sancta.Costs) return Sancta.Costs
--                    renames Sancta.Costs'Max;

   use Sancta.Criteria;
   use Sancta.Map.Paths;
   use Sancta.Tasks.Utils;

   use type Ada.Containers.Count_Type;
   use type Sancta.Ctree.Path_Trees.Cursor;
   use type Sancta.Costs;
   use type Sancta.Map.Location'Class;
   use type Tc.Lists.List;

   Id_Hash   : Tc.Lists.List;
   Id_Cost   : Id_Cost_Maps.Map;
   --  Profiler determined this to be a speed up.

   Global_Id : Natural := 1000;

   Global_Criterion : Sancta.Criteria.Enum_Criteria;

   ---------------------
   -- Min_Bots_Needed --
   ---------------------

   function Min_Bots_Needed (Jobs  : Tc.Lists.List;
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
      Jobs.Iterate (Check_Needed'Access);

      return Min;
   end Min_Bots_Needed;

   ---------------------
   -- Max_Bots_Needed --
   ---------------------

   function Max_Bots_Needed (Jobs  : Tc.Lists.List;
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
      Jobs.Iterate (Check_Needed'Access);

      return Max;
   end Max_Bots_Needed;

   ----------------------------
   -- Concurrent_Bots_Needed --
   ----------------------------

   function Concurrent_Bots_Needed (This    : Node_Data;
                                    Jobs    : Tc.Lists.List;
                                    Limit   : Network_Range) return Natural
   is
   begin
      declare
         Needed : Positive := 1 + Relays_Needed (This.Cost_From_Root, Limit);

         procedure Traversal (I : Path_Trees.Cursor) is
            Node : Node_Data renames Node_Data (I.Query.all);
         begin
            if I = This.Pos or else
              I.Parent.Child_Count > 1
            then
               if Intersect (Jobs, Node.Tasks_Here_And_Below).Is_Empty then
                  return;
               end if;
            end if;

            --  One extra bot needed for each branching:
            declare
               Children : constant Natural := I.Child_Count;
            begin
               if Children > 0 then
                  Needed := Needed + Children - 1;
               end if;
            end;

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

         return Needed;
      end;
   end Concurrent_Bots_Needed;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Map       :     Sancta.Map.Object'Class;
                      Navigator :     Tree_Navigator.Object'Class;
                      Tree      :     Path_Trees.Tree;
                      --  Nodes of Parallel.Node_Data
                      Bots      :     Positive;
                      Limit     :     Network_Range;
                      Criterion :     Sancta.Criteria.Enum_Criteria;
                      Stop      :     Sancta.Costs) return Sancta.Costs
   is

      All_Bots  : constant Positive      := Bots;
      All_Tasks : constant Tc.Lists.List := Navigator.Get_Tasks;

      Contexts  : Context_Sets.Set; -- THE MEAT IS INSIDE!!

      ------------
      -- Report --
      ------------

      procedure Report (Prefix : String;
                        Ctxt   : Recursive_Context;
                        Team   : Teams)
      is
         Relays : constant Natural :=
                    Relays_Needed (Team.Node.Cost_From_Root, Limit);
      begin
         if not Do_Report then
            return;
         end if;

         Log ("-- " & Rpad (Trim (Contexts.Length'Img), 5) & " -- " &
              Ctxt.Id'Img & " -" &
              Team.Id'Img & " - " &
              Prefix & " " &
              "Teams"  & Integer'Image (Integer (Ctxt.Teams.Length) + 1) & "; " &
              "Pend"   & Ctxt.Pending.Length'Img & "; " &
              "Depth"  & Team.Node.Cost_From_Root'Img & "; " &
              "Rels"   & Relays'Img & "; " &
              "Mobi"   & Natural'Image (Team.Bots - Relays) & "; " &
              "Tblw"   & Intersect
                (Ctxt.Pending, Team.Node.Tasks_Here_And_Below).Length'Img & "; " &
              "MinN"   & Min_Bots_Needed (Ctxt.Pending, Limit, Id_Cost)'Img & "; " &
              "MaxN"   & Max_Bots_Needed (Ctxt.Pending, Limit, Id_Cost)'Img & "; " &
              "Time"   & Team.Local_Time'Img,
              Debug, Log_Section);
      end Report;

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

      -----------------
      -- Target_Cost --
      -----------------

      function Target_Cost (Ctxt : Recursive_Context;
                            Team : Teams) return Sancta.Costs is
      begin
         case Criterion is
            when Minsum => return Ctxt.Sum;
            when Minmax => return Team.Local_Time;
            when Minave => return Ctxt.Ave / Sancta.Costs (All_Tasks.Length);
            when others => raise Program_Error with "Unimplemented";
         end case;
      end Target_Cost;

      -----------
      -- Shake --
      -----------

      procedure Shake (Ctxt : in out Recursive_Context) is
         Team : Teams := Ctxt.Teams.First_Element;

         -------------
         -- Move_It --
         -------------

         procedure Move_It (Team : in out Teams) is

            ----------------
            -- Accomplish --
            ----------------

            procedure Accomplish (I : Tc.Lists.Cursor) is
            begin
               Report ("TARGET", Ctxt, Team);
               Delete (Ctxt.Pending, Tc.Lists.Element (I).Get_Id);
               Ctxt.Ave        := Ctxt.Ave        + Team.Local_Time;
               Ctxt.Tasks_Done := Ctxt.Tasks_Done + 1;

               if not Team.Pos.Is_Leaf and then
                 Team.Bots <
                 Min_Bots_Needed (Intersect (Ctxt.Pending, Team.Node.Tasks_Here_And_Below),
                                  Limit,
                                  Id_Cost)
               then
                  Report ("RETREAT", Ctxt, Team);
                  Team.Direction := Up;
               end if;
            end Accomplish;

            -------------
            -- Go_Down --
            -------------

            procedure Go_Down is
               Relays : constant Natural :=
                          Relays_Needed (Team.Node.Cost_From_Root,
                                         Limit);
            begin
               if Team.Pos.Is_Leaf then
                  Team.Direction := Up;
                  Report ("LEAF", Ctxt, Team);
               else
                  Team.Pos := Team.Pos.First_Child;
                  declare

                     Step : constant Sancta.Costs :=
                              Team.Node.Cost_From_Root -
                                Node_Data (Team.Pos.Parent.Query.all).Cost_From_Root;
                  begin
                     Team.Local_Time := Team.Local_Time + Step;
                     Ctxt.Sum        := Ctxt.Sum +
                       Step * Sancta.Costs (Team.Bots - Relays);
                  end;
                  if Detailed or else Relays /=
                    Relays_Needed (Team.Node.Cost_From_Root, Limit)
                  then
                     Report ("DOWN", Ctxt, Team);
                  end if;
               end if;
            end Go_Down;

            -----------
            -- Go_Up --
            -----------

            procedure Go_Up is
               Relays : constant Natural :=
                          Relays_Needed (Team.Node.Cost_From_Root,
                                         Limit);
            begin
               if Team.Pos.Is_Root then
                  Report ("ROOT", Ctxt, Team);
                  if not Ctxt.Pending.Is_Empty then
                     --  We are at root, without ending... bad attempt,
                     --  annihilate this parallel universe.
                     raise Context_Extinguished;
                  end if;
               else
                  declare
                     Step   : constant Sancta.Costs :=
                                Team.Node.Cost_From_Root -
                                 Node_Data (Team.Pos.Parent.Query.all).Cost_From_Root;
                  begin
                     Team.Pos        := Team.Pos.Parent;
                     Team.Local_Time := Team.Local_Time + Step;
                     Ctxt.Sum        := Ctxt.Sum +
                       Step * Sancta.Costs (Team.Bots - Relays);

                     if Detailed or else Relays /=
                       Relays_Needed (Team.Node.Cost_From_Root, Limit)
                     then
                        Report ("UP  ", Ctxt, Team);
                     end if;
                  end;
                  if Ctxt.Teams.Is_Empty and then
                    Ctxt.Pending.Is_Empty and then
                    Team.Pos.Is_Root
                  then
                     Report ("DONE", Ctxt, Team);
                  end if;
               end if;
            end Go_Up;

            ---------------
            -- Do_Splits --
            ---------------

            procedure Do_Splits is

               Relays : constant Natural :=
                          Relays_Needed (Team.Node.Cost_From_Root,
                                         Limit);

               --------------------
               -- Warp_Idle_Team --
               --------------------

               procedure Warp_Idle_Team is
               begin
                  Ctxt.New_Id;
                  Team.Direction  := Wait;
                  Team.Local_Time := Sancta.Infinite;
                  Ctxt.Teams.Insert (Team);
                  Contexts.Insert (Ctxt);
                  Report ("WAIT", Ctxt, Team);
               end Warp_Idle_Team;

               ---------------------
               -- Warp_Downsplits --
               ---------------------

               procedure Warp_Downsplits is

                  -----------------
                  -- Warp_Launch --
                  -----------------

                  procedure Warp_Launch (Combo : Team_Array;
                                         Used  : Positive) is
                     use Asu;
                     Profile : Ustring;
                     Warp_Ctxt : Recursive_Context := Ctxt;

                     ------------------------
                     -- Compute_Split_Down --
                     ------------------------

                     procedure Compute_Split_Down (T : in out Teams) is
                        Step : constant Sancta.Costs :=
                                 T.Node.Cost_From_Root -
                                 Node_Data (T.Pos.Parent.Query.all).Cost_From_Root;
                        Relays : constant Natural :=
                                   Relays_Needed
                                     (Node_Data (T.Pos.Parent.Query.all).Cost_From_Root,
                                      Limit);
                     begin
--                          Log ("TIME STEP" & T.Local_Time'Img & Step'Img, Always);
                        T.Local_Time  := T.Local_Time + Step;
                        Warp_Ctxt.Sum := Warp_Ctxt.Sum +
                          Step * Sancta.Costs (T.Bots - Relays);

                        Report ("SPLIT+DOWN", Warp_Ctxt, T);

                     end Compute_Split_Down;

                     Combo_Rw  : Team_Array := Combo;
                     pragma Unreferenced (Combo);
                  begin
                     Warp_Ctxt.New_Id;

                     for I in Combo_Rw'Range loop
                        Append (Profile, Combo_Rw (I).Bots'Img);
                     end loop;

                     --  Log ("split" & (+Profile), Debug, Log_Section);
                     Report ("SPLIT" & (+Profile), Ctxt, Team);

                     for I in Combo_Rw'Range loop
                        Compute_Split_Down     (Combo_Rw (I));
                        Warp_Ctxt.Teams.Insert (Combo_Rw (I));
                     end loop;

                     --  Add any remainings as a waiting team:
                     if Team.Bots - Used > 0 then
                        declare
                           Extra_Team : Teams :=
                                          (Sancta.Infinite,
                                           Team.Bots - Used,
                                           Wait,
                                           Team.Pos,
                                           0);
                        begin
                           Extra_Team.New_Id;
                           Warp_Ctxt.Teams.Insert (Extra_Team);
                        end;
                     end if;

                     Contexts.Insert (Warp_Ctxt);
                  end Warp_Launch;

                  ----------------
                  -- Warp_Combo --
                  ----------------

                  procedure Warp_Combo (Combo : Team_Array;
                                        I     : Positive;
                                        C     : Path_Trees.Cursor;
                                        Used  : Natural;
                                        Avail : Integer) is
                  begin
                     --  Log ("warp combo" & I'Img & Used'Img & Avail'Img, Always);
                     --  End recursion
                     if not C.Has_Element then
                        if Used > 0 then
                           Warp_Launch (Combo, Used);
                        end if;
                        return;
                     elsif Used > 0 and then Avail <= Relays then
                        Warp_Launch (Combo, Used);
                        return;
                     end if;

                     --  Empty split here:
                     Warp_Combo (Combo,
                                 I + 1, C.Next_Sibling,
                                 Used,
                                 Avail);

                     --  All possible splits here:
                     if not Ctxt.Someone_Here_Or_Below (C) then
                        declare
                           Jobs : constant Tc.Lists.List :=
                                    Intersect
                                      (Ctxt.Pending,
                                       Node_Data (C.Query.all).Tasks_Here_And_Below);
                           Max  : constant Natural :=
                                    Max_Bots_Needed (Jobs, Limit, Id_Cost);
                           High :          Natural;
                        begin
                           --  Profiler pointed to Concurrent as hog:
                           if Avail <= Max then
                              High := Natural'Min (Avail, Max);
                           else
                              High := Natural'Min
                                (Avail,
                                 Concurrent_Bots_Needed
                                   (Node_Data (C.Query.all), Jobs, Limit));
                           end if;

                           for Use_Bots in
                             Min_Bots_Needed (Jobs, Limit, Id_Cost) .. High
                           loop
                              declare
                                 Warp_Team : Teams :=
                                               (Team.Local_Time,
                                                Use_Bots,
                                                Down,
                                                C,
                                                0);
                              begin
--                                   Log ("I TIME" & I'Img & Team.Local_Time'Img, Always);
                                 Warp_Team.New_Id;
                                 Warp_Combo (Combo & Warp_Team,
                                             I + 1, C.Next_Sibling,
                                             Used  + (Use_Bots - Relays),
                                             Avail - (Use_Bots - Relays));
                              end;
                           end loop;
                        end;
                     end if;
                  end Warp_Combo;
               begin
                  Warp_Combo ((1 .. 0 => <>),
                              1, Team.Pos.First_Child, 0, Team.Bots);
               end Warp_Downsplits;

               -------------------
               -- Attempt_Merge --
               -------------------

               procedure Attempt_Merge is
                  use Team_Sets;
                  I : Team_Sets.Cursor := Ctxt.Teams.Last;
                  J : Team_Sets.Cursor;
               begin
                  while Has_Element (I) loop
                     J := Previous (I);
                     declare
                        Other : constant Teams := Element (I);
                     begin
                        if Team.Pos = Other.Pos then
                           Ctxt.Teams.Delete (I);
                           --  Team.Local_Time is OK since it is the last one arriving
                           Team.Bots := Team.Bots + Other.Bots - Relays;
                           Report ("MERGE", Ctxt, Team);
                        end if;
                     end;
                     I := J;
                  end loop;
               end Attempt_Merge;

            begin
               case Team.Direction is


                  when Down =>
                     warp_Downsplits;


                  when Wait =>
                     raise Program_Error with "Shouldn't be reached!";


                  when Up =>
                     Report ("NEXUS", Ctxt, Team);
                     Attempt_Merge;

                     --  Create splits to go down
                     if not Intersect (Ctxt.Pending,
                                       Team.Node.Tasks_Here_And_Below).Is_Empty
                     then
                        Warp_Downsplits;
                     end if;

                     --  Or wait for someone else
                     if not Ctxt.Teams.Is_Empty and then
                       Ctxt.Someone_Strictly_Below (Team.Pos)
                     then
                        Warp_Idle_Team;
                     end if;

                     --  Or, if we are the only one or not at backloc, go up:
                     if Team.Pos.Is_Root then
                        Report ("ROOT", Ctxt, Team);
                     elsif not Ctxt.Someone_Strictly_Below (Team.Pos) then
                        Ctxt.New_Id;
                        Report ("PASSBY", Ctxt, Team);
                        Go_Up;
                        Ctxt.Teams.Insert (Team);
                        Contexts.Insert (Ctxt);
                     end if;
               end case;
            end Do_Splits;

         begin
            --  Execute tasks
            Intersect (Ctxt.Pending, Team.Node.Tasks_Here).Iterate (Accomplish'Access);

            --  Split?
            if Team.Pos.Child_Count > 1 then
               Do_Splits;
               raise Context_Extinguished;
            elsif Team.Direction = Down then
               Go_Down;
            elsif Team.Direction = Up then
               Go_Up;
            end if;

            if Target_Cost (Ctxt, Team) > Stop then
               Report ("OVERFLOW", Ctxt, Team);
               raise Cost_Exceeded;
            end if;
         end Move_It;

      begin
         Ctxt.Teams.Delete_First;
         Move_It (Team);
         Ctxt.Teams.Insert (Team);
      end Shake;

      Examined : Natural := 0;

   begin
      Global_Criterion := Criterion;

      if Id_Hash /= All_Tasks then
         Id_Hash := All_Tasks;
         Build_Tables;
      end if;

      Fix_Tree (Tree);

      --  Add first context
      Contexts.Insert
        (Recursive_Context'
           (Ada.Finalization.Controlled with
            Global_Id,
            All_Tasks,
            0,
            Team_Sets.To_Set
              (Teams'(0.0, Bots, Down, +Tree.Root, Global_Id)),
            0.0,
            0.0,
            Agpl.Containers.Integer_Sets.To_Set (Global_Id)));

      --  Process until death
      while not Contexts.First_Element.Is_Done loop
         Examined := Examined + 1;
         declare
            C : Recursive_Context := Contexts.First_Element;
         begin
            Contexts.Delete_First;
            begin
               Shake (C);
               Contexts.Insert (C);
            exception
               when Cost_Exceeded | Context_Extinguished =>
                  null;
            end;
         end;
      end loop;

      if Use_History then
         Log ("Explored states:" & Examined'Img, Debug, Log_Section);
         declare
            History : Ustring;
            Egrep   : Ustring;
            procedure H (I : Agpl.Containers.Integer_Sets.Cursor) is
            begin
               Asu.Append (History, Agpl.Containers.Integer_Sets.Element (I)'Img);
               Asu.Append (Egrep,   Agpl.Containers.Integer_Sets.Element (I)'Img);
               Asu.Append (Egrep,   " |");
            end H;
         begin
            Asu.Append (History, "History:");
            Asu.Append (Egrep,   "egrep '(");
            Contexts.First_Element.History.Iterate (H'Access);
            Asu.Append (Egrep,   " DATA )'");
            Log (+History, Debug, Log_Section);
            Log (+Egrep,   Debug, Log_Section);
         end;
      end if;

      pragma Assert (Contexts.First_Element.Teams.First_Element.Bots = All_Bots);

      return Contexts.First_Element.Current_Cost (Criterion);
   end Evaluate;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Teams) return Boolean is
   begin
      return L.Local_Time < R.Local_Time;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Recursive_Context) return Boolean is
   begin
      case Global_Criterion is
         when Minsum =>
            return L.Sum < R.Sum;
         when Minmax =>
            return L.Earliest_Time < R.Earliest_Time;
         when Minave =>
            return L.Current_Cost (Minave) < R.Current_Cost (Minave)
              or else (L.Current_Cost (Minave) = R.Current_Cost (Minave)  and then
                       L.Earliest_Time < R.Earliest_Time);
         when others =>
            raise Program_Error with "Unimplemented";
      end case;
   end "<";

   -----------------
   -- Latest_Time --
   -----------------

   function Latest_Time (This : Recursive_Context) return Sancta.Costs is
   begin
      return This.Teams.Last_Element.Local_Time;
   end Latest_Time;

   -------------------
   -- Earliest_Time --
   -------------------

   function Earliest_Time (This : Recursive_Context) return Sancta.Costs is
   begin
      return This.Teams.First_Element.Local_Time;
   end Earliest_Time;

   ------------------
   -- Current_Cost --
   ------------------

   function Current_Cost (This      : Recursive_Context;
                          Criterion : Sancta.Criteria.Enum_Criteria)
                          return      Sancta.Costs
   is
   begin
      case Criterion is
         when Minsum =>
            return This.Sum;
         when Minmax =>
            return This.Latest_Time;
         when Minave =>
            if This.Tasks_Done > 0 then
               return This.Ave / Sancta.Costs (This.Tasks_Done);
            else
               return 0.0;
            end if;
         when others =>
            raise Program_Error;
      end case;
   end Current_Cost;

   -------------
   -- Is_Done --
   -------------

   function Is_Done (This : Recursive_Context) return Boolean is
   begin
      return This.Teams.Length = 1 and then
      This.Pending.Is_Empty and then
      This.Teams.First_Element.Pos.Is_Root;
   end Is_Done;

   ----------
   -- Node --
   ----------

   function Node (This : Teams) return access Node_Data is
   begin
      return Node_Data (This.Pos.Update.all)'Access;
   end Node;

   ---------------------------
   -- Someone_Here_Or_Below --
   ---------------------------

   function Someone_Here_Or_Below (This : Recursive_Context;
                                   Pos  : Path_Trees.Cursor)
                                   return Boolean
   is
      Hi_There : exception;
      procedure Check_Below (I : Team_Sets.Cursor) is
         Idx : Path_Trees.Cursor := Team_Sets.Element (I).Pos;
      begin
         loop
            if Idx = Pos then
               raise Hi_There;
            end if;
            exit when Idx.Is_Root;
            Idx := Idx.Parent;
         end loop;
      end Check_Below;
   begin
      This.Teams.Iterate (Check_Below'Access);
      return False;
   exception
      when Hi_There =>
         return True;
   end Someone_Here_Or_Below;

   ----------------------------
   -- Someone_Strictly_Below --
   ----------------------------

   function Someone_Strictly_Below (This : Recursive_Context;
                                    Pos  : Path_Trees.Cursor)
                                    return Boolean
   is
      Hi_There : exception;
      procedure Check_Below (I : Team_Sets.Cursor) is
         Idx : Path_Trees.Cursor := Team_Sets.Element (I).Pos;
      begin
         if Idx.Is_Root then
            return;
         else
            Idx := Idx.Parent;
         end if;

         loop
            if Idx = Pos then
               raise Hi_There;
            end if;
            exit when Idx.Is_Root;
            Idx := Idx.Parent;
         end loop;
      end Check_Below;
   begin
      This.Teams.Iterate (Check_Below'Access);
      return False;
   exception
      when Hi_There =>
         return True;
   end Someone_Strictly_Below;

   ------------
   -- New_Id --
   ------------

   procedure New_Id (This : in out Recursive_Context) is
   begin
      Global_Id := Global_Id + 1;
      This.Id   := Global_Id;

      if Use_History then
         This.History.Insert (Global_Id);
      end if;
   end New_Id;

   ------------
   -- New_Id --
   ------------

   procedure New_Id (This : in out Teams) is
   begin
      Global_Id := Global_Id + 1;
      This.Id   := Global_Id;
   end New_Id;

end Sancta.Ctree.Parallel.Brute;
