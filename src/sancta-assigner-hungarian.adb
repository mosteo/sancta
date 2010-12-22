with Sancta.Agent.Utils; use Sancta.Agent.Utils;
with Sancta.Tasks.Utils; use Sancta.Tasks.Utils;

--  with Sancta.Cost_Matrix;

with Hungarian_Solver;

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Assigner.Hungarian is

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return Assignment.Object
   is
      pragma Unreferenced (This);

      package Hu is new Hungarian_Solver.Solver (Natural (Agents.Length),
                                                 Natural (Tasks.Length));

      A : constant Ac.Vectors.Vector := +Agents;
      T : constant Tc.Vectors.Vector := +Tasks;
      H : Hu.Problem;
      C : Hu.Cost_Matrix;
   begin
      if A.Is_Empty or else T.Is_Empty then
         return Assignment.Object (Sancta.Assignment.Empty_Object);
      end if;

--      Cost_Matrix.Object (Costs).Print;

      --  Create hungarian problem
      for I in C'Range (1) loop
         for J in C'Range (2) loop
            if Costs.Get_Cost (A.Element (Natural (I)).Get_Name,
                               Sancta.Tasks.No_Task,
                               T.Element (Natural (J)).Get_Id) >
              Sancta.Costs (Hu.Costs'Last)
            then
               C (I, J) := Hu.Costs'Last;
               Log ("Overflow cost: " & A.Element (Natural (I)).Get_Name &
                    ": No_Task --" & T.Element (Natural (J)).Get_Id'Img,
                    Warning, Log_Section);
            else
               C (I, J) := Hu.Costs
                 (Costs.Get_Cost (A.Element (Natural (I)).Get_Name,
                  Sancta.Tasks.No_Task,
                  T.Element (Natural (J)).Get_Id));
            end if;
         end loop;
      end loop;
      H.Create (C);
      H.Solve;
      --  H.Print_Assignment;
      --  Solve and convert back to assignment
      declare
         S : constant Hu.Solution_Array := H.Solution;
         R :          Assignment.Object;
      begin
         for I in A.First_Index .. A.Last_Index loop
            R.Set_Agent (A.Element (I)); -- Add agent in any case or we leak them
            if S (Hu.Worker_Index (I)) in 1 .. Hu.Job_Index (T.Length) then
               R.Add       (A.Element (I),
                            T.Element (Natural (S (Hu.Worker_Index (I)))));
               --  and add the task if won
            end if;
         end loop;
         return R;
      end;
   end Assign;

   ------------
   -- Assign --
   ------------

   function Assign
     (Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Cost_Cache.Object'Class)
      return      Assignment.Object
   is
      Dummy : constant Object := (Assigner.Object with null record);
   begin
      return Dummy.Assign (Agents, Tasks, Costs);
   end Assign;

end Sancta.Assigner.Hungarian;
