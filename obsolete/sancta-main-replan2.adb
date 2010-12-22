with Sancta.Agent_Proxy;
with Sancta.Config;
with Sancta.Debug; use Sancta.Debug;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

with Sancta;
with Sancta.Tasks.Containers;
with Agpl.Optimization.Concorde;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_Io; use Ada.Text_Io;

--  The purpose of this program is to compare cost incurred when tasks are
--  added in subsequent steps after initial solution.

--  In this example, a task is added each time another one is finished.
--  We specify the initial number of tasks and the additional tasks

--  <program> <initial tasks> <additional tasks>

--  The window size is the number of tasks performed before replanning.

--  There's just one agent performing tasks.

procedure Sancta.Main.Replan2 is

   Rand   : Generator;

   Pi     : constant Float := 3.1415926535;

   Xmax   : constant Float := 100.0;
   Ymax   : constant Float := 100.0;
   Amax   : constant Float := Pi * 2.0;

   use type Sancta.Costs;
   use type Types.Real;
   use type Ada.Containers.Count_Type;

   Bot : Agent_Proxy.Object;

   --------------
   -- Cost_TSP --
   --------------

   function Cost_TSP (Current   : in Sancta.Tasks.Object'Class;
                      Jobs      : in Sancta.Tasks.Containers.Lists.List;
                      Remain    : in Sancta.Tasks.Containers.Lists.List;
                      Window    : in Positive := 1) return Sancta.Costs
   is
      --  We will compute costs according to the best TSP solution available.
      --  Before replanning, we perform @window@ steps.
      use Agpl.Optimization.Concorde;
      use Sancta.Tasks.Containers;

      function Get_At (L : in List; Pos : in Positive) return Sancta.Tasks.Object'Class is
         I : Cursor   := First (L);
         P : Positive := 1;
      begin
         while Has_Element (I) loop
            if P = Pos then
               return Element (I);
            else
               P := P + 1;
               Next (I);
            end if;
         end loop;

         raise Program_Error;
      end Get_At;

   begin
      Put_Line ("VISIBLE:" & Jobs.Length'Img & "; REMAIN:" & Remain.Length'Img);

      if Jobs.Is_Empty then
         return 0.0;
      elsif Jobs.Length = 1 then
         Put_Line ("Cost remaining last job: " &
                   To_String (Bot.Get_Cost (Current, Jobs.First_Element)));
         return Bot.Get_Cost (Current, Jobs.First_Element);
      end if;

      declare
         Jobs_B : List := Jobs;
         Rem_B  : List := Remain;
         Costs  : Cost_Matrix (1 .. Cities (Jobs.Length) + 1,
                               1 .. Cities (Jobs.Length) + 1);
         Start  : constant Start_Matrix := (1 => Costs'Last);
      begin
         Jobs_B.Append (Current); -- Last one is the current pos
         --  Prepare cost matrix:
         declare
            From : Cursor := Jobs_B.First;
            To   : Cursor;
            F, T : Cities;
         begin
            F := Costs'First;
            while Has_Element (From) loop
               To := Jobs_B.First;
               T  := Costs'First (2);
               while Has_Element (To) loop
                  Costs (F, T) :=
                    Optimization.Concorde.Costs
                      (Sancta.Costs'Floor (Bot.Get_Cost (Element (From), Element (To))));
                  Next (To);
                  T := T + 1;
               end loop;
               Next (From);
               F := F + 1;
            end loop;
         end;
         --  Solve:
         Print_Problem (Costs);
         declare
            Result : constant Result_Matrix := Solve_MTSP (Start, Costs, No_Return => True);
            Tour   : constant Normal_Tour   := Create (Start, Result);
            Acum   : Sancta.Costs := 0.0;
            N_Done : constant Stages :=
                       Stages'Min (Stages (Window), Stages (Jobs.Length));
         begin
            Print_Solution (Costs, Start, Result, True);
            --  Remove job for start position:
            Jobs_B.Delete_Last;
            --  Get costs and solve descendent problem:
            for I in 1 .. N_Done loop
               Acum := Acum + Sancta.Costs (Costs (City (Tour, 1, I), City (Tour, 1, I + 1)));
               --  Remove visited job:
               declare
                  Del : Cursor := Jobs_B.Find (Get_At (Jobs, Positive (City (Tour, 1, I + 1))));
               begin
                  Jobs_B.Delete (Del);
               end;
               --  Add a new discovered job:
               if not Rem_B.Is_Empty then
                  Jobs_B.Append (Rem_B.First_Element);
                  Rem_B.Delete_First;
               end if;
            end loop;

            declare
               New_Current : constant Sancta.Tasks.Object'Class :=
                               Get_At (Jobs,
                                       Positive (City (Tour, 1, N_Done + 1)));
            begin
               return Acum + Cost_TSP (New_Current,
                                       Jobs_B,
                                       Rem_B);
            end;
         end;
      end;
   end Cost_TSP;

   -----------------
   -- Cost_Greedy --
   -----------------
   --  TSPend means that when no new tasks remain, we use optimal planning.
   function Cost_Greedy (Current : in Sancta.Tasks.Object'Class;
                         Jobs    : in Sancta.Tasks.Containers.Lists.List;
                         Remain  : in Sancta.Tasks.Containers.Lists.List;
                         TSPend  : in Boolean) return Sancta.Costs
   is
      --  We will compute costs according to the greedy solution available.
      use Sancta.Tasks.Containers;

      Best_Pos  : Cursor;
      Best_Cost : Sancta.Costs := Sancta.Costs'Last;

      Jobs_B    : List     := Jobs;
      Rem_B     : List     := Remain;
      I         : Cursor   := Jobs_B.First;
   begin
      if not Has_Element (I) then
         return 0.0;
      end if;

      while Has_Element (I) loop
         declare
            Cost : constant Sancta.Costs := Bot.Get_Cost (Current, Element (I));
         begin
            if Cost < Best_Cost then
               Best_Pos  := I;
               Best_Cost := Cost;
            end if;
         end;
         Next (I);
      end loop;

      declare
         New_Pos : Sancta.Tasks.Object'Class := Element (Best_Pos);
      begin
         Jobs_B.Delete (Best_Pos); -- Remove already performed

         if not Rem_B.Is_Empty then
            Jobs_B.Append (Rem_B.First_Element);
            Rem_B.Delete_First;
         end if;

         if Rem_B.Is_Empty and then TSPend then
            return Best_Cost + Cost_TSP (New_Pos,
                                         Jobs_B,
                                         Rem_B);
         else
            return Best_Cost + Cost_Greedy (New_Pos,
                                            Jobs_B,
                                            Rem_B,
                                            TSPend);
         end if;
      end;
   end Cost_Greedy;

begin
   --  Common configuration.
   Config.Init (Log_Level => Trace.Debug);
   --  Enable_Section (Config.Tracer, Sancta.Traderbot_Sim.Log_Section);

   Reset (Rand);

   --  Retrieve working parameters
   if Argument_Count /= 2 then
      Put_Line ("Usage: " & Command_Name & " <Initial tasks> <Additional tasks>");
      return;
   end if;

   for Repeats in 1 .. 100 loop

      --  Create the array of tasks:
      declare
         Initial_Tasks    : constant Positive := Positive'Value (Argument (1));
         Additional_Tasks : constant Natural  := Natural'Value (Argument (2));
--         Window_Size      : constant Positive := Positive'Value (Argument (3));
         Jobs             : Sancta.Tasks.Containers.Lists.List;
         Remain           : Sancta.Tasks.Containers.Lists.List;
      begin
         for I in 1 .. Initial_Tasks + Additional_Tasks + 1 loop
            declare
               Job              : Sancta.Tasks.Goto_Pose.Object;
               S1, S2, S3, Smin : Sancta.Costs;
            begin
               Job.Assign_Id;
               Job.Set_Pose ((Types.Real (Random (Rand) * Xmax),
                              Types.Real (Random (Rand) * Ymax),
                              Types.Angle (Random (Rand) * Amax)));
               if I <= Initial_Tasks then
                  Jobs.Append (Job);
               elsif I <= Initial_Tasks + Additional_Tasks then
                  Remain.Append (Job);
               else
                  --  If I is the initial task, compute solutions:
                  S1 := Cost_Greedy (Job, Jobs, Remain, TSPend => False);
                  S2 := Cost_Greedy (Job, Jobs, Remain, TSPend => True);
                  S3 := Cost_TSP (Job, Jobs, Remain, Window => 1);
                  Smin := Sancta.Costs'Min (S1, Sancta.Costs'Min (S2, S3));

                  Put_Line ("[Sol]" &
                            S1'Img &
                            S2'Img &
                            S3'Img &
                            Sancta.Costs'Image (S1 / Smin) &
                            Sancta.Costs'Image (S2 / Smin) &
                            Sancta.Costs'Image (S3 / Smin));

               end if;
            end;
         end loop;
      end;

   end loop;

end Sancta.Main.Replan2;
