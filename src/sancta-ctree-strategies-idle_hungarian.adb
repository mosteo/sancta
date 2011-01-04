with Sancta.Ctree.Assigner.Hungarian;
with Sancta.Ctree.Strategies.Idle_Greedy;
with Sancta.Ctree.Weak_Grouping;
with Sancta.Agent;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Ctree.Strategies.Idle_Hungarian is

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class)
   is
      pragma Unreferenced (This);
   begin
      Ass := Perform (Ass.Get_Agents, Tasks, Costs, Links);
   end Perform;

   -------------
   -- Perform --
   -------------

   function Perform (Agents : Ac.Lists.List;
                     Tasks  : Tc.Lists.List;
                     Costs  : Sancta.Cost_Cache.Object'Class;
                     Links  : Connectivity_Matrix.Object'Class)
                     return   Sancta.Assignment.Object
   is
      Ass :          Sancta.Assignment.Object := Sancta.Assignment.Create (Agents);
      Grp : constant Weak_Grouping.Object     := Weak_Grouping.Create (Agents,
                                                                       Links);
      A   :          Ac.Lists.List;

      use Ac.Lists;
      procedure Find_Idle_Groups (I : Cursor) is
         Ag : constant Sancta.Agent.Object'Class := Element (I);
      begin
         if (not Ag.Has_Tasks) and then
           Grp.Get_Mates (Ag.Get_Name).Get_Non_Idle_Agents.Is_Empty
         then
            A.Append (Ag);
            Log ("Idle_Hungarian:" & Ag.Get_Name & " is in IDLE group", Never);
         else
            Log ("Idle_Hungarian:" & Ag.Get_Name & " is in BUSY group", Never);
         end if;
      end Find_Idle_Groups;

   begin
      Log ("AT IDLE_HUNGARIAN" & A.Length'Img, Debug, Log_Section);

      Ass.Get_Agents.Iterate (Find_Idle_Groups'Access);

      --  hungarian fill-in
      if not A.Is_Empty and then not Tasks.Is_Empty then
         Ass.Set_Agents
           (Assigner.Hungarian.Assign
              (A, Tasks, Costs, Links).Get_Agents);
      end if;

      --  Propagate any assigned
      Assigner.Copy_To_Linked (Ass, A, Links);

      --  Greedy for remnants
      A.Clear;
      Ass.Get_Agents.Iterate (Find_Idle_Groups'Access);
      if not A.Is_Empty then
         Ass := Idle_Greedy.Perform
           (A, Tasks, Costs, Links, Propag => True);
      end if;

      return Ass;
   end Perform;

end Sancta.Ctree.Strategies.Idle_Hungarian;
