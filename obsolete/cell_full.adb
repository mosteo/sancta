with Sancta.Agent_Proxy;
with Sancta.Draw;
with Sancta.Tasks.Grid_Goal;
with Sancta.Traderbot_Sim;
with Sancta.Types; use Sancta.Types;
use Sancta;

with Agpl.Conversions; use Agpl.Conversions;
with Sancta;
with Sancta.Agent;
with Sancta.Agent.Containers;
with Sancta.Assigner.Greedy_Exhaustive;
with Sancta.Assigner.Greedy_Best_Pair_Tail;
with Sancta.Assigner.Greedy_Fifo_Tail;
with Sancta.Assignment;
with Sancta.Cost_Matrix;
with Agpl.Gdk;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Utils; use Sancta.Tasks.Utils;
with Agpl.Random;
with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

procedure Cell_Full is

   package Agent_Lists renames Sancta.Agent.Containers.Lists;
   use type Sancta.Costs;

   procedure Usage is
   begin
      Put_Line ("<results file> <# places> <iters>");
      Put_Line ("Output: " &
                "[Lago MinMax] [Lago MinTim] [Lago MinMix] [Lago MinSum] " &
                "[Tail MinMax] [Tail MinTim] [Tail MinMix] [Tail MinSum] " &
                "[Trad MinMax] [Trad MinTim] [Trad MinMix] [Trad MinSum] " &
                "[Tail]");
   end Usage;

   function S is new To_Str (Sancta.Costs);
   function S is new To_Str (Float);

   pragma Warnings (Off);
   type Algorithms is (Lago_Minmax,
                       Lago_Mintim,
                       Lago_Minmix,
                       Lago_Minsum,
                       Tail_Minmax,
                       Tail_Mintim,
                       Tail_Minmix,
                       Tail_Minsum,
                       Trad_Minmax,
                       Trad_Mintim,
                       Trad_Minmix,
                       Trad_Minsum,
                       Fifo);
   pragma Warnings (On);

   type Cost_Array is array (Algorithms) of Sancta.Assignment_Criteria;
   --  Well use the weights as costs

   procedure Print (F : in out File_Type; C : Cost_Array) is
      use Strings;
   begin
      for I in C'Range loop
         Put (F,
              Rpad (S (C (I).Minmax_Weight), 9) &
              Rpad (S (C (I).Minsum_Weight), 9));
      end loop;
      New_Line (F);
      Flush (F);
   end Print;

   procedure Print (C : Cost_Array) is
      use Strings;
   begin
      for I in C'Range loop
         Put (Rpad (S (C (I).Minmax_Weight), 9) &
              Rpad (S (C (I).Minsum_Weight), 9));
      end loop;
      New_Line;
   end Print;

   procedure Announce (S : String) is
   begin
      Put_Line ("******************************************");
      Put_Line ("*                " & S);
      Put_Line ("******************************************");
   end Announce;

   Criteria : constant array (1 .. 4) of Sancta.Assignment_Criteria :=
                ((1.0, 0.0),
                 (1.0, 0.00001),
                 (1.0, 1.0),
                 (0.0, 1.0));

   Acum_Costs : Cost_Array := (others => (0.0, 0.0));

   package Grid renames Tasks.Grid_Goal;

   F : File_Type;

   Places : Positive;
   Iters  : Positive;
begin
   if Argument_Count /= 3 then
      Usage;
      return;
   end if;

   Create (F, Name => Argument (1), Mode => Out_File);

   Places := Positive'Value (Argument (2));
   Iters  := Positive'Value (Argument (3));

   for Iter in 1 .. Iters loop
      declare
         Poses     : Sancta.Types.Pose_Array (1 .. Places);
         Row_Costs : Cost_Array := (others => (0.0, 0.0));
         Row_Idx   : Algorithms := Algorithms'First;
         Agents    : Agent_Lists.List;
         Cm        : Sancta.Cost_Matrix.Object;
      begin
         loop
            Grid.Clear;

            --  Create places:
            for I in Poses'Range loop
               Poses (I).X := Real (Random.Get_Float (0.0, 100.0));
               Poses (I).Y := Real (Random.Get_Float (0.0, 100.0));
               Poses (I).A := 0.0;
            end loop;

            --  Create graph:
            Grid.Create_From_Poses (Poses, 20.0);

            exit when Grid.Graph.Is_Connected;

            Put_Line ("Discarding not connected graph");
         end loop;

         --  Position agents
         declare
            Vertices : constant Grid.Grid_Graphs.Vertex_Vectors.Vector :=
                         Grid.Graph.Get_Vertices;
         begin
            for I in 1 .. 4 loop
               declare
                  Ag : Agent_Proxy.Object;
               begin
                  Ag.Set_Name (Types.Agent_Names (I));
                  Ag.Set_Pose
                    (Vertices.Element
                       (Grid.Grid_Graphs.Vertex_Index
                          (Random.Get_Integer
                             (Integer (Vertices.First_Index),
                              Integer (Vertices.Last_Index)))).Data);
                  Agents.Append (Ag);
               end;
            end loop;
         end;

         Cm := Sancta.Cost_Matrix.Create_With_Start
           (Agents,
            To_List (Tasks.Grid_Goal.Mission));

         Announce ("LAGO");

         --  LAGO  --
         for I in Criteria'Range loop
            declare
               Asser : Sancta.Assigner.Greedy_Exhaustive.Object;
               Assed : Sancta.Assignment.Object;
               MM_Cost  : Sancta.Costs;
               Ms_Cost  : Sancta.Costs;
            begin
               Asser.Criterion := Criteria (I);
--                 Assed := Asser.Assign (Agents,
--                                        To_List (Grid.Mission),
--                                        Cm);
               MM_Cost := Assed.Get_Cost (Cm, (1.0, 0.0));
               MS_Cost := Assed.Get_Cost (Cm, (0.0, 1.0));

               Put (F, S (MM_Cost) & " " & S (MS_Cost) & " ");
               Flush (F);
               Acum_Costs (Row_Idx).Minmax_Weight :=
                 Acum_Costs (Row_Idx).Minmax_Weight + Float (Mm_Cost);
               Acum_Costs (Row_Idx).Minsum_Weight :=
                 Acum_Costs (Row_Idx).Minsum_Weight + Float (Ms_Cost);
               Row_Costs  (Row_Idx) := (Float (Mm_Cost), Float (Ms_Cost));

               --  Draw.Draw_Assignment (Assed);

               Row_Idx := Algorithms'Succ (Row_Idx);
            end;
         end loop;
         --  LAGO ENDS HERE --

         Announce ("TAIL");

         --  TAIL  --
         for I in Criteria'Range loop
            declare
               Asser : Sancta.Assigner.Greedy_Best_Pair_Tail.Object;
               Assed : Sancta.Assignment.Object;
               MM_Cost  : Sancta.Costs;
               Ms_Cost  : Sancta.Costs;
            begin
               Asser.Criterion := Criteria (I);
--                 Assed := Asser.Assign (Agents,
--                                        To_List (Grid.Mission),
--                                        Cm);
--                 MM_Cost := Assed.Get_Cost (Cm, (1.0, 0.0));
--                 MS_Cost := Assed.Get_Cost (Cm, (0.0, 1.0));

               Put (F, S (MM_Cost) & " " & S (MS_Cost) & " ");
               Flush (F);
               Acum_Costs (Row_Idx).Minmax_Weight :=
                 Acum_Costs (Row_Idx).Minmax_Weight + Float (Mm_Cost);
               Acum_Costs (Row_Idx).Minsum_Weight :=
                 Acum_Costs (Row_Idx).Minsum_Weight + Float (Ms_Cost);
               Row_Costs  (Row_Idx) := (Float (Mm_Cost), Float (Ms_Cost));

               --  Draw.Draw_Assignment (Assed);

               Row_Idx := Algorithms'Succ (Row_Idx);
            end;
         end loop;
         --  TAIL ENDS HERE --

         Announce ("TRAD");

         --  TRAD  --
         for I in Criteria'Range loop
            declare
               Asser : Traderbot_Sim.Object;
               Assed : Sancta.Assignment.Object;
               MM_Cost  : Sancta.Costs;
               Ms_Cost  : Sancta.Costs;
            begin
               Asser.Set_Costs (Cm);
               declare procedure Add_Agent (I : Agent_Lists.Cursor) is
                  begin
                     Asser.Add_Agent (Agent_Lists.Element (I));
                  end Add_Agent;
               begin
                  Agents.Iterate (Add_Agent'Access);
               end;
               Asser.Set_Criterion (Criteria (I));

               declare
                  Mission : Sancta.Tasks.Containers.Vectors.Vector :=
                              Grid.Mission;
               begin
                  while not Mission.Is_Empty loop
                     declare
                        Chosen : constant Positive :=
                                   Random.Get_Integer
                                     (1, Integer (Mission.Length));
                     begin
                        Asser.Add_Task (Mission.Element (Chosen));
                        Mission.Delete (Chosen);
                        Put (".");
                     end;
                  end loop;
                  New_Line;
               end;
               Put_Line ("Starting rounds");
               for Auction_Rounds in 1 .. 360 loop
                  declare
                     Changes : Traderbot_Sim.Outcomes;
                     use Traderbot_Sim;
                  begin
                     Asser.Auction_Random_Task (Changes);
                     Put ("%");
                  end;
               end loop;
               New_Line;

               Assed := Asser.To_Assignment;
               MM_Cost := Assed.Get_Cost (Cm, (1.0, 0.0));
               MS_Cost := Assed.Get_Cost (Cm, (0.0, 1.0));

               Put (F, S (MM_Cost) & " " & S (MS_Cost) & " ");
               Flush (F);
               Acum_Costs (Row_Idx).Minmax_Weight :=
                 Acum_Costs (Row_Idx).Minmax_Weight + Float (Mm_Cost);
               Acum_Costs (Row_Idx).Minsum_Weight :=
                 Acum_Costs (Row_Idx).Minsum_Weight + Float (Ms_Cost);
               Row_Costs  (Row_Idx) := (Float (Mm_Cost), Float (Ms_Cost));

               --  Draw.Draw_Assignment (Assed);

               Row_Idx := Algorithms'Succ (Row_Idx);
            end;
         end loop;
         --  TRAD ENDS HERE --

         Announce ("FIFO");

         --  FIFO  --
         declare
            Asser    : Sancta.Assigner.Greedy_Fifo_Tail.Object;
            Assed    : Sancta.Assignment.Object;
            Mm_Cost  : Sancta.Costs;
            Ms_Cost  : Sancta.Costs;
         begin
--              Assed := Asser.Assign (Agents,
--                                     To_List (Grid.Mission),
--                                     Cm);
            Mm_Cost := Assed.Get_Cost (Cm, (1.0, 0.0));
            Ms_Cost := Assed.Get_Cost (Cm, (0.0, 1.0));

            Put (F, S (Mm_Cost) & " " & S (Ms_Cost) & " ");
            Flush (F);
            Acum_Costs (Row_Idx).Minmax_Weight :=
              Acum_Costs (Row_Idx).Minmax_Weight + Float (Mm_Cost);
            Acum_Costs (Row_Idx).Minsum_Weight :=
              Acum_Costs (Row_Idx).Minsum_Weight + Float (Ms_Cost);
            Row_Costs  (Row_Idx) := (Float (Mm_Cost), Float (Ms_Cost));

            --  Draw.Draw_Assignment (Assed);

            --  Row_Idx := Algorithms'Succ (Row_Idx);
            --  Would out of range
         end;
         --  FIFO ENDS HERE --

         New_Line (F);

         Put_Line ("AVERAGES TILL ITERATION" & Iter'Img);
         declare
            Avg : Cost_Array := (others => (0.0, 0.0));
         begin
            for I in Avg'Range loop
               Avg (I).Minmax_Weight := Acum_Costs (I).Minmax_Weight / Float (Iter);
               Avg (I).Minsum_Weight := Acum_Costs (I).Minsum_Weight / Float (Iter);
            end loop;
            Print (Avg);
         end;
         Put_Line ("CURRENT ITERATION COSTS");
         Print (Row_Costs);
      end;
   end loop;

   --  Dump averages

   New_Line (F);
   Put_Line (F, "************* SUMMMMMARY AVERAGED **************");
   declare
      Avg : Cost_Array := (others => (0.0, 0.0));
   begin
      for I in Avg'Range loop
         Avg (I).Minmax_Weight := Acum_Costs (I).Minmax_Weight / Float (Iters);
         Avg (I).Minsum_Weight := Acum_Costs (I).Minsum_Weight / Float (Iters);
      end loop;
      Print (F, Avg);
   end;

   Close (F);

exception
   when E : others =>
      if Is_Open (F) then
         Close (F);
      end if;
      Put_Line (Standard_Error, "Main: " & Report (E));
end Cell_Full;

