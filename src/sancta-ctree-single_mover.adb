with Ada.Containers,
     Agpl.Strings,
     Sancta.Ctree.Tasks.Go,
--     Sancta.Ctree.Utils,
     Sancta.Debug2,
--     Sancta.Located_Agent,
     Sancta.Tasks.Goto_Pose,
     Sancta.Tasks.Utils,
     Sancta.Types.Operations;

package body Sancta.Ctree.Single_Mover is

   package Stasks renames Sancta.Tasks;

   use type
       Ada.Containers.Count_Type,
       Map.Location'Class,
       Sancta.Tasks.Object'Class;

   -------------
   -- Go_Fwrd --
   -------------

   function Go_Fwrd (This : Object;
                     Loc  : Map.Location'Class) return Stasks.Handle.Object is
   begin
      return
        Stasks.Handle.Set
          (Sancta.Ctree.Tasks.Go.Go_Fwrd (This.M.Ref.Nearest_Pose (Loc)));
   end Go_Fwrd;

   -------------
   -- Go_Back --
   -------------

   function Go_Back (This : Object;
                     Loc  : Map.Location'Class) return Stasks.Handle.Object is
   begin
      return
        Stasks.Handle.Set
          (Sancta.Ctree.Tasks.Go.Go_Back (This.M.Ref.Nearest_Pose (Loc)));
   end Go_Back;

   ------------
   -- Create --
   ------------

   function Create (Team           : Assignment.Object;
                    Plan           : Tc.Lists.List;
                    Here_Threshold : Costs;
                    Near_Threshold : Costs;
                    M              : Map.Smart.Object) return Object
   is
      use Ac.Lists;
      function Max return Natural is
         Y : Natural := 0;
         procedure Max (I : Cursor) is
         begin
            Y := Natural'Max (Y, Natural'Value (Element (I).Get_Name));
         end Max;
      begin
         Team.Get_Agents.Iterate (Max'Access);
         return Y;
      end Max;

      This : Object := (Team_Size      => Max,
                        Pending        => Plan,
                        Here_Threshold => Here_Threshold,
                        Near_Threshold => Near_Threshold,
                        Task_State     => Pending,
                        M              => M,
                        others         => <>);

      procedure Create (I : Cursor) is
         Bot : Agent_Proxy.Object renames Agent_Proxy.Object (Element (I));
      begin
         Log ("Creating " & Bot.Get_Name, Debug, Log_Section);
         This.Bots (Natural'Value (Bot.Get_Name)) :=
           (Bot,
            Free,
            Map.Location_Handle.Set (M.Ref.Nearest_Location (Bot.Get_Pose)));
         Log ("At pose " &
              Debug2.To_String
                (This.Bots (Natural'Value (Bot.Get_Name)).Bot.Get_Pose),
              Debug, Log_Section);
         Log ("At loc " &
              This.Bots (Natural'Value (Bot.Get_Name)).Loc.Get.Image,
              Debug, Log_Section);
      end Create;
   begin
      Team.Get_Agents.Iterate (Create'Access);
      return This;
   end Create;

   ----------
   -- Step --
   ----------

   procedure Step
     (This  : in out Object;
      Team  :        Assignment.Object;
      Plan  :        Tc.Lists.List;
      Tree  :        Tree_Navigator.Object'Class;
      Links :        Connectivity_Matrix.Object'Class;
      Alloc :    out Assignment.Object;
      Event :    out Step_Outputs)
   is
      -------------------
      -- Compute_Input --
      -------------------

      function Compute_Input (I : Positive) return  Robot_Inputs
      is
         function Strained (I : Positive) return Boolean is
            --  If we have good link with at least one previous relay, we are ok.
         begin
            --  ONLY CONSIDER PREVIOUS
            --  return Links.Is_Weak (This.Name (I - 1), This.Name (I));

            --  CONSIDER ALL PREVIOUS
            for J in 1 .. I - 1 loop
               if not Links.Is_Weak (This.Name (J), This.Name (I)) then
                  return False;
               end if;
            end loop;
            return True;
         end Strained;

      begin

         case This.Role (I) is
            when Root =>
               return (Strained => False);
            when Head =>
               return (Strained => Strained (I));
            when Link =>
               return (Strained => Strained (I));
         end case;
      end Compute_Input;

      --------------------
      -- Compute_Inputs --
      --------------------

      procedure Compute_Inputs
      is
      begin
         for I in This.Inputs'Range loop
            This.Inputs (I) := Compute_Input (I);
         end loop;
      end Compute_Inputs;

      ------------
      -- Action --
      ------------

      type Whole_States is (Pending_Relay, Pending_Free, Done_Relay, Done_Free);

      States : constant array (Task_States, Robot_States) of Whole_States :=
                 (Pending => (Relay => Pending_Relay, Free => Pending_Free),
                  Done    => (Relay => Done_Relay,    Free => Done_Free));

      function Action (Bot  : String)
      return Robot_Actions
      is
         I : constant Positive := Positive'Value (Bot);
      begin
         if This.Role (I) /= Root then
            case States (This.Task_State, This.Bots (I).State) is
               when Pending_Relay =>
                  return (Detail => Stopped, Kind => Hold);
               when Pending_Free =>
                  if This.Inputs (I).Strained then
                     if This.Role (I) = Head then
                        Event := Head_Strained;
                        This.Task_State := Done;
                        Log ("HEAD STRAINED", Always);
--                          if
--                            Tree.Branch (This.Pending.First_Element).Length =
--                            This.M.Ref.Best_Path
--                              (This.M.Ref.Nearest_Location
--                                   (This.Bots (This.Bots'First).Bot.Get_Pose),
--                               This.M.Ref.Nearest_Location
--                                 (Stasks.Goto_Pose.Object
--                                    (This.Pending.First_Element).Pose)).Path.Length
--                          then
--                             Event := Mission_Failed;
--                          end if;
                     else
                        This.Bots (I).State := Relay;
                     end if;
                     return (Detail => Stopping, Kind => Hold);
                  elsif
                    (This.Role (I) = Head or else
                     This.Dist (I, I + 1) >= Near)
                    and then
                    (This.Dist (I - 1, I) <= Near or else
                     This.Role (I - 1) = Root or else
                     This.Bots (I - 1).State = Relay)
                  then
                     return (Detail => Advancing,
                             Kind   => Move,
                             Goal   => This.Go_Fwrd
                               (Map.Next (This.Branch, This.Bots (I).Loc.Get)));
                  else
                     return (Detail => Waiting,
                             Kind => Hold);
                  end if;
               when Done_Relay =>
                  if This.Dist (I, I + 1) <= Near then
                     This.Bots (I).State := Free;
                     return (Detail => Departing,
                             Kind   => Move,
                             Goal   => This.Go_Back
                               (Map.Prev (This.Branch, This.Bots (I).Loc.Get)));
                  else
                     return (Detail => Stopped, Kind => Hold);
                  end if;
               when Done_Free =>
                  if
                    (This.Role (I) = Head or else
                     This.Dist (I, I + 1) <= Near) and then
                    This.Dist (I - 1, I) >= Near
                  then
                     return (Detail => Backtracking,
                             Kind   => Move,
                             Goal   => This.Go_Back
                               (Map.Prev (This.Branch, This.Bots (I).Loc.Get)));
                  else
                     return (Detail => Waiting, Kind => Hold);
                  end if;
            end case;
         else
            return (Detail => Root, Kind => Hold);
         end if;
         --  Fallback, shouldn't be reached

--           Log ("Bot:" & I'Img &
--                "; TS: " & This.Task_State'Img &
--                "; RS: " & This.Bots (I).State'Img, Error, Log_Section);
--           raise Program_Error with "Uncontrolled state";
      end Action;

      -----------------
      -- Team_Action --
      -----------------

      function Team_Action return Assignment.Object is
         use Agpl.Strings;
         Result : Assignment.Object;

         procedure Check (I : Positive) is
            Act : constant Robot_Actions := Action (This.Bots (I).Bot.Get_Name);
         begin
            Log ("Bot" & I'Img & ": " &
                 States (This.Task_State, This.Bots (I).State)'Img &
                 "[" & To_Lower (Act.Detail'Img) & "]",
                 Debug, Log_Section);

            Result.Set_Agent (This.Bots (I).Bot);
            case Act.Kind is
            when Hold =>
               Result.Clear_Tasks (This.Bots (I).Bot.Get_Name);
            when Move =>
               Result.Set_Task (This.Bots (I).Bot.Get_Name,
                                Act.Goal.Get);
            end case;
         end Check;
      begin
         if This.Task_State = Done and then This.Ancestor.Is_Valid then
            Log ("Common ancestor is:" & This.Ancestor.Get.Image,
                 Debug, Log_Section);
         end if;

         for I in This.Bots'Range loop
            Check (I);
         end loop;

         Log ("8", Never);

         return Result;
      end Team_Action;

      -----------------
      -- Update_Bots --
      -----------------

      procedure Update_Bots is
         use Ac.Lists;
         procedure Update_Bots (J : Cursor) is
            Bot : Agent_Proxy.Object renames Agent_Proxy.Object (Element (J));
            I   : constant Positive := Positive'Value (Bot.Get_Name);
            Bot_Loc : constant Map.Location'Class :=
                        This.M.Ref.Nearest_Location (Bot.Get_Pose);
         begin
            This.Bots (I).Bot := Bot;
            if not This.Branch.Contains (This.Bots (I).Loc.Get) then
               --  This is a fault condition; shouldn't happen unless
               --   at startup when robots may be out of the branch. In this
               --   case, force them into the nearest branch location
               This.Bots (I).Loc.Set
                 (This.M.Ref.Nearest_Location_In_Path (Bot_Loc, This.Branch));
               Log ("Bot" & I'Img & " forced into branch",
                    Warning, Log_Section);
            end if;

            case This.Task_State is
               when Pending =>
                  if
                    This.Bots (I).Loc.Get /= This.Branch.Last_Element and then
                    Bot_Loc = Map.Next (This.Branch, This.Bots (I).Loc.Get)
                  then
                     This.Bots (I).Loc.Set
                       (This.M.Ref.Nearest_Location (Bot.Get_Pose));
                  end if;
               when Done =>
                  if
                    This.Bots (I).Loc.Get /= This.Branch.First_Element and then
                    Bot_Loc = Map.Prev (This.Branch, This.Bots (I).Loc.Get)
                  then
                     This.Bots (I).Loc.Set
                       (This.M.Ref.Nearest_Location (Bot.Get_Pose));
                  end if;
            end case;
            Log ("Bot" & I'Img & " pos: " &
                 Debug2.To_String (This.Bots (I).Bot.Get_Pose),
                 Debug, Log_Section);
            Log ("Bot" & I'Img & " loc: " & This.Bots (I).Loc.Get.Image,
                 Debug, Log_Section);
         exception
            when E : others =>
               Log ("Updating bot: " & Report (E), Warning, Log_Section);
         end Update_Bots;
      begin
         Team.Get_Agents.Iterate (Update_Bots'Access);
      end Update_Bots;

      -----------------------
      -- Check_Goal_Change --
      -----------------------

      procedure Check_Goal_Change is
         use Sancta.Tasks.Utils,
             Tc.Lists;
      begin
         case This.Task_State is
            when Pending =>
               if
                 Intersect (Plan, This.Pending).First_Element /=
                 This.Pending.First_Element
               then
                  Log ("Plan interrupted!", Debug, Log_Section);
                  Log ("pend/plan:" & This.Pending.Length'Img & "/" &
                       Plan.Length'Img,
                       Debug, Log_Section);
                  This.Task_State := Done;
                  This.Ancestor.Set
                    (Map.Common_Ancestor
                       (This.Branch,
                        Tree.Branch (Plan.First_Element)));
                  Log ("Ancestor set to: " & This.Ancestor.Get.Image,
                       Debug, Log_Section);
               else
                  This.Branch := Tree.Branch (This.Pending.First_Element);
               end if;
            when Done =>
               if not This.Pending.Is_Empty then
                  --  Determine backtrack point
                  This.Ancestor.Set
                    (Map.Common_Ancestor
                       (This.Branch,
                        Tree.Branch (This.Pending.First_Element)));
               else
                  This.Ancestor.Set
                    (Map.Next (This.Branch, This.base.Loc.Get));
               end if;
         end case;

         --  If task ordering changes, keep only pending but in new order.
         This.Pending := Intersect (Plan, This.Pending);
         Log ("Pending tasks:" & This.Pending.Length'Img, Debug, Log_Section);
      end Check_Goal_Change;

      ------------------------
      -- Check_Goal_Reached --
      ------------------------

      procedure Check_Goal_Reached is
         use Tc.Lists;
      begin
         if
           This.Bots (This.Bots'Last).Bot.Finished (This.Pending.first_element)
         then
            Log ("Goal reached", Debug, Log_Section);
            This.Pending.Delete_First;
            This.Task_State := Done;
            This.St.Mark_Task_Completed;
         else
            Log
              ("Goal distance:" &
               Costs'Image
                 (Costs
                    (Types.Operations.Distance
                       (This.Bots (This.Bots'Last).Bot.Get_Pose,
                        Stasks.Goto_Pose.Object
                          (This.Pending.First_Element).Pose))),
               Debug, Log_Section);
         end if;

         --  Check passing by goals!
         if Natural (This.Pending.Length) > 1 then
            declare
               I : Cursor := Next (This.Pending.First);
               J : Cursor;
            begin
               while Has_Element (I) loop
                  J := Next (I);
                  if This.Bots (This.Bots'Last).Bot.Finished (Element (I)) then
                     Log ("Passing-by goal reached", Debug, Log_Section);
                     This.Pending.Delete (I);
                     This.St.Mark_Task_Completed;
                  end if;
                  I := J;
               end loop;
            end;
         end if;

         if This.Pending.Is_Empty then
            Log ("All tasks cleared", Informative, Log_Section);
            This.Print_Stats;
            This.St_Printed := False;
         end if;
      end Check_Goal_Reached;

      ----------------------------
      -- Check_Ancestor_Reached --
      ----------------------------

      procedure Check_Ancestor_Reached is
         use Tc.Lists;
      begin
         for I in This.Bots'First + 1 .. This.Bots'Last - 1 loop
            --  Ensure all robots have reached branch
            if
              Map.Path_Fields.Slice
             (This.Branch,
              This.Bots (This.Bots'Last).Loc.Get,
              This.Branch.Last_Element).Contains (This.Ancestor.Get) and then
              not Map.Path_Fields.Slice
              (This.Branch,
               This.Branch.First_Element,
               This.Ancestor.Get).Contains (This.Bots (I).Loc.Get)
            then
               Log ("Waiting for misplaced links!", Warning, Log_Section);
               return;
            end if;
         end loop;

         if
           not This.Pending.Is_Empty and then
           Map.Path_Fields.Slice
             (This.Branch,
              This.Bots (This.Bots'Last).Loc.Get,
              This.Branch.Last_Element).Contains (This.Ancestor.Get)
         then
            This.Task_State := Pending;
            This.Branch     := Tree.Branch (This.Pending.First_Element);
         end if;
      end Check_Ancestor_Reached;

      procedure Make_Tree is
      begin
         This.Tt.Set_Parent (This.Name (This.Bots'First),
                             This.Name (This.Bots'First));
         for I in This.Bots'Range loop
            if I /= This.Bots'First then
               This.Tt.Set_Parent (Child  => This.name (I),
                                   Parent => This.Name (I - 1));
            end if;
         end loop;
      end Make_Tree;

   begin
      This.Update_Stats;

      if This.Pending.Is_Empty and then This.Replegated then
         Event := Mission_Completed;
         This.Print_Stats;
      else
         Event := None;
      end if;

      This.Tt := Ctree.Team_Tree.Object (Ctree.Team_Tree.Create (From => Team));
      Make_Tree;

      Log ("1", Never);
      if not This.Branch.Is_Empty then
         Update_Bots;
      end if;

      Log ("2", Never);
      Check_Goal_Change;

      if Event /= Mission_Completed then

         if This.Task_State = Pending then
            Log ("3", Never);
            if not This.Pending.Is_Empty then
               Check_Goal_Reached;
            end if;
         else
            Log ("4", Never);
            Check_Ancestor_Reached;
         end if;

         Log ("5", Never);
         Compute_Inputs;
         Log ("6", Never);
         Alloc := Team_Action;
         Log ("7", Never);

      else

         Alloc.Clear;

      end if;

      if not This.St_Inited then
         This.St_Inited := True;
         This.St.Init (Links);
      else
         This.St.Update (Alloc,
                         Links,
                         This.Pending);
      end if;

   end Step;

   ----------
   -- Role --
   ----------

   function Role (This : Object;
                  Bot  : Positive) return Robot_Roles
   is
   begin
      if Bot = This.Team_Size then
         return Head;
      elsif Bot = 1 then
         return Root;
      else
         return Link;
      end if;
   end Role;

   ----------
   -- Name --
   ----------

   function Name (This : Object;
                  Bot  : Positive) return String
   is
   begin
      return This.Bots (Bot).Bot.Get_Name;
   end Name;

   -------------------
   -- Pending_Tasks --
   -------------------

   function Pending_Tasks (This : Object) return Tc.Lists.List is
   begin
      return This.Pending;
   end Pending_Tasks;

   ---------------
   -- Team_Tree --
   ---------------

   function Team_Tree (This : Object) return Ctree.Team_Tree.Object is
   begin
      return This.Tt;
   end Team_Tree;

   ------------------
   -- Update_Stats --
   ------------------

   procedure Update_Stats (This : in out Object) is
   begin
      case This.Task_State is
         when Pending =>
            This.Time_Fwrd := This.Time_Fwrd + This.Cron.Elapsed;
         when Done =>
            This.Time_Back := This.Time_Back + This.Cron.Elapsed;
      end case;
      This.Cron.Reset;
   end Update_Stats;

   -----------------
   -- Print_Stats --
   -----------------

   procedure Print_Stats (This : in out Object) is
      Total       : constant Duration := This.Time_Fwrd + This.Time_Back;
      Pct_Fwrd    : constant Duration :=
                      Duration (This.Time_Fwrd * 100.0) / Total;
      Pct_Back    : constant Duration :=
                      Duration (This.Time_Back * 100.0) / Total;
   begin
      if not This.St_Printed then
         This.St.Print;
         Log ("Single_Mover stats: " &
              "FWRD:" & Costs (This.Time_Fwrd)'Img &
              "; BACK:" & Costs (This.Time_Back)'Img &
              ";" & Natural (Pct_Fwrd)'Img & "% /" &
              Natural (Pct_Back)'Img & "%",
              Always, Log_Section);
         This.St_Printed := True;
      end if;
   end Print_Stats;

   ----------
   -- Base --
   ----------

   function Base (This : Object) return Robot_Context is
   begin
      return This.Bots (This.Bots'First);
   end Base;

   ----------
   -- Dist --
   ----------

   function Dist (This : Object;
                  I, J : Positive) return Relative_Locations
   is
      function Rel_Dist (C : Costs) return Relative_Locations is
      begin
         Log (I'Img & J'Img & "-dist is:" & C'Img, Debug, Log_Section);
         if C <= This.Here_Threshold then
            return Here;
         elsif C <= This.Near_Threshold then
            return Near;
         else
            return Far;
         end if;
      end Rel_Dist;

      Path_Between : Map.Path;
   begin
      if J > This.Bots'Last then
         raise Constraint_Error with "Bad robot";
      end if;

      if J /= I + 1 then
         raise Constraint_Error with "Non-consecutive robots";
      end if;

      --  Check inversion!!
      if
        This.M.ref.Get_Cost_Across_Path
        (This.Bots (This.Bots'First).Bot.Get_Pose,
         This.Bots (I).Bot.Get_Pose,
         Map.Path_Fields.Slice
           (This.Branch,
            This.Base.Loc.Get,
              This.Bots (I).Loc.Get)) >
        This.M.ref.Get_Cost_Across_Path
        (This.Bots (This.Bots'First).Bot.Get_Pose,
         This.Bots (J).Bot.Get_Pose,
         Map.Path_Fields.Slice
           (This.Branch,
            This.Base.Loc.Get,
              This.Bots (J).Loc.Get))
      then
         Log ("Bots" & I'Img & J'Img & " are reversed!", Debug, Log_Section);
         --  Not too reliable, lets omit it for now...
         --  return Here;
      end if;

      Path_Between :=
        Map.Path_Fields.Slice (This.Branch,
                               This.Bots (I).Loc.Get,
                               This.Bots (J).Loc.Get);
      Log (I'Img & J'Img & "-len:" & Path_Between.Length'Img,
           Debug, Log_Section);

      declare
         C : constant Relative_Locations :=
               Rel_Dist
                 (This.M.Ref.Get_Cost_Across_Path
                    (This.Bots (I).Bot.Get_Pose,
                     This.Bots (J).Bot.Get_Pose,
                     Path_Between));
      begin
         Log (I'Img & J'Img & "-rel: " & C'Img,
              Debug, Log_Section);

         return C;
      end;
   end Dist;

   ----------------
   -- Replegated --
   ----------------

   function Replegated (This : Object) return Boolean is
   begin
      for I in This.Bots'First .. This.Bots'Last - 1  loop
         if This.Dist (I, I + 1) > Here then
            return False;
         end if;
      end loop;
      return True;
   end Replegated;

end Sancta.Ctree.Single_Mover;
