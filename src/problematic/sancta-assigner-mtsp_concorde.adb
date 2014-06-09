

with Sancta.Agent.Utils;
with Sancta.Tasks.Starting_Pose;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Concorde;
with Agpl; use Agpl;

package body Sancta.Assigner.MTSP_Concorde is

   use Agpl.Optimization;

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return      Assignment.Object
   is
      Jobs   :          Sancta.Tasks.Containers.Vectors.Vector;
      Ag     : constant Sancta.Agent.Object'Class := Agents.First_Element;
      Result :          Assignment.Object;
      Agvect : constant Agent.Containers.Vectors.Vector :=
                 Agent.Utils.To_Vector (Agents);

      --------------------
      -- Agent_For_Task --
      --------------------

      function Agent_For_Task (T : in Sancta.Tasks.Object'Class)
                               return Agent.Object'Class
      is
      begin
         if T in Sancta.Tasks.Starting_Pose.Object then
            for I in Agvect.First_Index .. Agvect.Last_Index loop
               if Agvect.Element (I).Get_Name =
                 Sancta.Tasks.Starting_Pose.Object (T).Get_Name then
                  return Agvect.Element (I);
               end if;
            end loop;
            raise Program_Error;
         else
            return Ag;
         end if;
      end Agent_For_Task;

   begin
      --  Create the tasks vector
      --  Use a starting task for each agent:
      declare
         use Sancta.Agent.Containers.Lists;
         I : Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            Jobs.Append (Sancta.Tasks.Starting_Pose.Create (Element (I).Get_Name));
            Next (I);
         end loop;
      end;

      --  Add the tasks:
      declare
         use Sancta.Tasks.Containers.Lists;
         I : Cursor := Tasks.First;
      begin
         while Has_Element (I) loop
            if Element (I) in Sancta.Tasks.Starting_Pose.Object then
               raise Constraint_Error with "No Starting_Pose tasks allowed";
            end if;
            Jobs.Append (Element (I));
            Next (I);
         end loop;
      end;

      --  Create the concorde things and solve
      declare
         use Optimization.Concorde;
         Start : Start_Matrix (1 .. Salesmen (Agents.Length));
         C     : Optimization.Concorde.Cost_Matrix :=
                   Cost_Matrices.Create (Cities (Jobs.Length),
                                         Cities (Jobs.Length));
         use Cost_Cache;
      begin
         for I in Start'Range loop
            Start (I) := Cities (I);
         end loop;

         for I in Jobs.First_Index .. Jobs.Last_Index loop
            declare
               --  Choose the agent for the starting task
               --  If is not a Starting_Task, any will do:
               Apt_Agent : constant Agent.Object'Class :=
                             Agent_For_Task (Jobs.Element (I));
               Co : Sancta.Costs;
            begin
               for J in Jobs.First_Index .. Jobs.Last_Index loop
                  if I = J then
                     Co := Sancta.Infinite;
                  elsif Jobs.Element (I) in Sancta.Tasks.Starting_Pose.Object'Class then
                     if Jobs.Element (J) in Sancta.Tasks.Starting_Pose.Object'Class then
                        Co := Sancta.Infinite;
                     else
                        Co := Costs.Get_Cost (Apt_Agent.Get_Name,
                                             Sancta.Tasks.No_Task,
                                             Jobs.Element (J).Get_Id);
                     end if;
                  else
                     if Jobs.Element (J) in Sancta.Tasks.Starting_Pose.Object'Class then
                        Co := Costs.Get_Cost (Apt_Agent.Get_Name,
                                              Jobs.Element (I).Get_Id,
                                              Sancta.Tasks.No_Task);
                     else
                        Co := Costs.Get_Cost (Apt_Agent.Get_Name,
                                              Jobs.Element (I).Get_Id,
                                              Jobs.Element (J).Get_Id);
                     end if;
                  end if;

                  if Co < Infinite then
                     C.Set (Cities (I), Cities (J), Concorde.Costs (Co));
                  else
                     C.Set (Cities (I), Cities (J), Concorde.Inf);
                  end if;
               end loop;
            end;
         end loop;

         --  Optimization.Concorde.Print_Problem (C);

         declare
            --  Solve
            Tour : constant Normal_Tour :=
                     Create
                       (Start,
                        Solve_Mtsp
                          (Start,
                           C,
                           No_Return => not This.Return_To_Base));
            use Sancta.Agent.Containers.Lists;
            A : Cursor := Agents.First;
         begin
            --  Reconstruct agents
            for I in 1 .. Tour.Last loop
               declare
                  New_Agent : Sancta.Agent.Object'Class := Element (A);
               begin
                  Next (A);
                  --  Set the name of the starting task, I think the tour
                  --  can be rotated (error somewhere????)
                  declare
                     Name : constant String :=
                              Sancta.Tasks.Starting_Pose.Object
                                (Jobs.Element
                                   (Positive (Tour.City (I, 1)))).Get_Name;
                  begin
                     if Name /= New_Agent.Get_Name then
                        Log ("Agent mismatch! " & Name & " should be " &
                             New_Agent.Get_Name, Error, Log_Section);
                        raise Constraint_Error;
                     end if;
                  end;

                  --  Assign tasks, skipping the forced starting pose
                  for J in 2 .. Tour.Last (I) loop
                     New_Agent.Add_Task (Jobs.Element
                                           (Positive (Tour.City (I, J))));
                  end loop;
                  Result.set_Agent (New_Agent);
               end;
            end loop;
         end;
      end;

      return Result;
   end Assign;

   function Assign
     (Agents         : in Agent.Containers.Lists.List;
      Tasks          : in Sancta.Tasks.Containers.Lists.List;
      Costs          : in Sancta.Cost_Cache.Object'Class;
      Return_To_Base : in Boolean := False)
      return      Assignment.Object
   is
      Dummy : Object (Return_To_Base);
   begin
      return Dummy.Assign (Agents, Tasks, Costs);
   end Assign;

end Sancta.Assigner.MTSP_Concorde;
