with Sancta.Ctree.Weak_Grouping;
with Sancta.Located_Agent;
with Sancta.Types.Operations;
with Sancta.Agent.Containers;
with Agpl.Strings; use Agpl.Strings;

use Sancta;

package body Sancta.Ctree.Stats is

   package Ac renames Sancta.Agent.Containers;
   use type Sancta.Tasks.Task_Id;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
   begin
      for I in 1 .. 8 loop
         Log (I'Img &
              This.Clusters.Val (I'Img)'Img &
              Integer'Image (This.Clusters.Val (I'Img) * 100 / This.Iters),
              Always);
      end loop;

      if This.Completed_Tasks > 0 then
         Log ("MinMax(T): "       & To_String (Float (This.Cron.Elapsed), 0) &
           "; MinAve(T): "     & To_String (This.Values (Min_Ave_Time) /
               Float (This.Completed_Tasks), 0) &
           " (" & To_String (This.Completed_Tasks) & ")" &
           "; MinMax(O):"       & To_String (This.Values (Min_Max_Odom)) &
           "; MinSum(O):"       & To_String (This.Values (Min_Sum_Odom)) &
           "; Deallocs:"    & To_String (This.Values (Deallocations), 0) &
           "; Topochanges:" & To_String (This.Values (Topo_Changes), 0),
           Always, Log_Section);
      else
         Log ("MinMax(T): "       & To_String (Float (This.Cron.Elapsed), 0) &
           "; MinAve(T): 0 "     &
           " (" & To_String (This.Completed_Tasks) & ")" &
           "; MinMax(O):"       & To_String (This.Values (Min_Max_Odom)) &
           "; MinSum(O):"       & To_String (This.Values (Min_Sum_Odom)) &
           "; Deallocs:"    & To_String (This.Values (Deallocations), 0) &
           "; Topochanges:" & To_String (This.Values (Topo_Changes), 0),
           Always, Log_Section);
      end if;
   end Print;

   ----------
   -- Init --
   ----------

   procedure Init (This  : in out Object;
                   Links :        Connectivity_Matrix.Object'Class)
   is
   begin
      This.Cron.Reset;

      This.Prev_Links := Connectivity_Matrix.Object (Links);

      for I in 1 .. 8 loop
         This.Clusters.Add (I'Img, 0);
      end loop;
   end Init;

   ------------
   -- Update --
   ------------

   procedure Update
     (This          : in out Object;
      Ass           :        Sancta.Assignment.Object;
      Links         :        Connectivity_Matrix.Object'Class;
      Pending_Tasks :        Tc.Lists.List)
   is
      function To_Set (List : Tc.Lists.List) return Tc.Key_Sets.Set is
         Set : Tc.Key_Sets.Set;
         procedure Append (I : Tc.Lists.Cursor) is
         begin
            Set.Include (Tc.Lists.Element (I).Get_Id);
         end Append;
      begin
         List.Iterate (Append'Access);
         return Set;
      end To_Set;

      Prev_Assigned_Tasks : Tc.Key_Sets.Set;
      Curr_Assigned_Tasks : Tc.Key_Sets.Set;
      Curr_Pending_Tasks  : Tc.Key_Sets.Set;

      Preemptions : Natural;
   begin
      --  TOPOLOGY
      if
        not This.Prev_Links.Same_Topology (Connectivity_Matrix.Object (Links))
      then
         This.Values (Topo_Changes) := This.Values (Topo_Changes) + 1.0;
      end if;
      This.Prev_Links := Connectivity_Matrix.Object (Links);

      --  DEALLOCATIONS
      Prev_Assigned_Tasks := To_Set (This.Prev_Ass.Get_All_First_Tasks);
      Curr_Assigned_Tasks := To_Set (Ass.Get_All_First_Tasks);
      Curr_Pending_Tasks  := To_Set (Pending_Tasks);

      declare
         use Tc.Key_Sets;
         Remain : constant Tc.Key_Sets.Set :=
           (Prev_Assigned_Tasks and Curr_Pending_Tasks) - Curr_Assigned_Tasks;
      begin
         Preemptions := Natural (Remain.Length);
      end;

      if Preemptions > 0 then
         Log ("Preemptions:" & Preemptions'Img, Debug, Log_Section);
      end if;
      This.Values (Deallocations) :=
        This.Values (Deallocations) + Float (Preemptions);

      This.Compute_Odometry (Ass);

      This.Prev_Ass := Ass;

      --  ITERATIONS
      This.Iters := This.Iters + 1;

      --  CLUSTERCOUNT HISTOGRAM
      This.Clusters.Add
        (Weak_Grouping.Create (Ass, Links).Num_Groups'Img);
   end Update;

   ----------------------
   -- Compute_Odometry --
   ----------------------

   procedure Compute_Odometry (This : in out Object;
                               Ass  :        Sancta.Assignment.Object)
   is
      procedure Do_It (I : Ac.Lists.Cursor) is
         use Sancta.Types.Operations;
         Bot  : constant located_agent.Object'class :=
                  Located_Agent.Object'Class (Ac.Lists.Element (I));
         Dist :          Float        := 0.0;
      begin
         if not This.Bot_Odom.Contains (Bot.Get_Name) then
            This.Bot_Odom.Insert (Bot.Get_Name, 0.0);
         end if;

         if This.Prev_Ass.Contains (Bot.Get_Name) then
            Dist := Float
              (Distance
                 (Located_Agent.Object'Class
                    (This.Prev_Ass.Get_Agent (Bot.Get_Name)).Get_Pose,
                  Bot.Get_Pose));
            Log ("Bot " & Bot.Get_Name & " delta odom is" & To_String (Dist),
                 Debug, Log_Section);

            This.Values (Min_Sum_Odom) := This.Values (Min_Sum_Odom) + Dist;

            This.Bot_Odom.Include (Bot.Get_Name,
                                   This.Bot_Odom.Element (Bot.Get_Name) + Dist);

            This.Values (Min_Max_Odom) :=
              Float'Max (This.Bot_Odom.Element (Bot.Get_Name),
                         This.Values (Min_Max_Odom));
         end if;
      end Do_It;
   begin
      Ass.Get_Agents.Iterate (Do_It'Access);
   end Compute_Odometry;

   -------------------------
   -- Mark_Task_Completed --
   -------------------------

   procedure Mark_Task_Completed (This : in out Object) is
   begin
      This.Completed_Tasks := This.Completed_Tasks + 1;
      This.Values (Min_Ave_Time) :=
        This.Values (Min_Ave_Time) + Float (This.Cron.Elapsed);
   end Mark_Task_Completed;

   ---------------------
   -- Completed_Tasks --
   ---------------------

   function Completed_Tasks (This : Object) return Natural is
   begin
      return This.Completed_Tasks;
   end Completed_Tasks;

   --------------------
   -- Current_Values --
   --------------------

   function Current_Values (This : Object) return Metrics_Array is
   begin
      return This.Values;
   end Current_Values;

end Sancta.Ctree.Stats;
