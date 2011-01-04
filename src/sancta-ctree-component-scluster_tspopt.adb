with Sancta.Ctree.Component.Nctypes,
     Sancta.Ctree.Connectivity_Matrix.Utils,
     Sancta.Ctree.Strategies.Idle_Hungarian,
     Sancta.Ctree.Strategies.Propagate,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory,
     Sancta.Tasks.Utils;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Ctree.Component.Scluster_Tspopt is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ---------
   -- Run --
   ---------

   procedure Run
     (This  : in out Proto;
      Bots  : in out Assignment.Object;
      Jobs  : in out Tc.Lists.List;
      Costs :        Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class)
   is
      use type Tc.Lists.List;
   begin
      if This.Prev_Jobs /= Jobs then
         Log ("S-Cluster reset", Debug, Log_Section);
         This.Sclusters := Connectivity_Matrix.Object (Links);
         This.Prev_Jobs := Jobs;
      else
         This.Sclusters.Set_Umbral (Links.Get_Umbral);
         Connectivity_Matrix.Utils.Merge_Keep_Weak
           (This.Sclusters, Connectivity_Matrix.Object (Links));
      end if;

--        Log ("CLUSTERS", Always);
--        Links.Print;
--
--        Log ("S-CLUSTERS", Always);
--        This.Sclusters.Print;

      declare
         Base_Name : constant String := Sancta.Image (This.Base_Name);
         Ass_0, Ass_1, Ass_2, Ass_3  : Assignment.Object;
         Scluster_Tree               : constant Connectivity_Matrix.Object :=
                                         This.Sclusters.Spanning_Tree;
      begin
         Ass_0 := Bots;
         if Ass_0.Contains (Base_Name) then
            Ass_0.Remove_Agent (Base_Name);
--              Log ("Base " & Base_Name & " removed.", Debug, Log_Section);
         end if;

         This.Tsp.Assign (Ass_0.Get_Agents,
                          Jobs, Jobs,
                          Costs,
                          Scluster_Tree,
                          This.Prev_Ass,
                          Ass_1);
         Ass_1.Merge_Missing_Robots (Ass_0);

         Ass_2 := Strategies.Propagate.Perform (Ass_1, Scluster_Tree);
         Ass_2.Merge_Missing_Robots (Ass_1);

         Ass_3 := Strategies.Idle_Hungarian.Perform (Ass_2.Get_Agents,
                                                     Jobs,
                                                     Costs,
                                                     Scluster_Tree);
         Ass_3.Merge_Missing_Robots (Ass_2);
         Ass_3.Merge_Missing_Robots (Bots); -- Only the base, in theory

         Bots := Ass_3;
         if Bots.Contains (Base_Name) then
            Bots.Clear_Tasks (Base_Name);
            Bots.Set_Tasks (Base_Name, This.Tsp.Get_Plan);
         end if;
         This.Prev_Ass := Bots;

--           Log ("ASS 1", Always);
--           Ass_1.Print_Assignment;
--           Log ("ASS 2", Always);
--           Ass_2.Print_Assignment;
--           Log ("ASS 3", Always);
--           Ass_3.Print_Assignment;
      end;
   end Run;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Comp_Config)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      if This.Exists (Option_Base_Name) then
         This.Inner.Base_Name := Sancta.Value (This.Option (Option_Base_Name));
      end if;

      return Sancta.Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      Next := Clock + 0.2;

      if not This.Ready then
         This.Ready :=
           This.Exists (Requires_Team) and then
           This.Exists (Requires_Links) and then
           This.Exists (Requires_Costs) and then
           This.Exists (Requires_Pending_Tasks);
      end if;

      if This.Ready then
         declare
            Bots : Assignment.Object :=
                     Ctypes.Teams (This.Input (Requires_Team)).Team;
            Jobs : Tc.Lists.List :=
                     Ctypes.Task_List
                       (This.Input (Requires_Pending_Tasks)).Tasks;
         begin
            This.Inner.Run
              (Bots,
               Jobs,
               Ctypes.Cost_Cache (This.Input (Requires_Costs)).Ptr.all,
               Nctypes.Links (This.Input (Requires_Links)).Links);

            This.Output (Provides_Team,
                         Ctypes.Teams'(Team => Bots));

            This.Output (Provides_Scluster,
                         Nctypes.Links'(Links => This.Inner.Sclusters));
         end;
      end if;
   end Run;

end Sancta.Ctree.Component.Scluster_Tspopt;
