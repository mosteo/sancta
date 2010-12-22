with Sancta.Agent_Proxy;
with Sancta.Auctioner;
with Sancta.Config;
with Sancta.Costs;
with Sancta.Debug;
with Sancta.Tasks.Goto_Pose;
with Sancta.Traderbot;
with Sancta.Types;

with Agpl.Command_Line;
with Sancta;
with Sancta.Agent.Containers;
with Agpl.Gdk.Managed;
with Sancta.Tasks.Containers;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_Io; use Ada.Text_Io;

--  Generate

procedure Sancta.Main.Compare1draw is
   Agents : Sancta.Agent.Containers.Lists.List;
   Tasks  : Sancta.Tasks.Containers.Lists.List;
   Bot    : Agent_Proxy.Object;
   Go     : Sancta.Tasks.Goto_Pose.Object;

   Rand   : Generator;

   Xmax   : constant Float := 100.0;
   Ymax   : constant Float := 100.0;
   Amax   : constant Float := 3.14159265 * 2.0;
   Pi     : constant Float := 3.1415926535;

   use type Sancta.Costs;
   use type Types.Real;

   Iterations : constant := 1; -- #repetitions for average a single experiment.

   Full_Result : File_Type;
   Avg_Result  : File_Type;

   type Starting_Places is (Close, Lateral, Corners, Random);

   Acum_Optimal : Sancta.Costs := 0.0;
   Acum_Trading : Sancta.Costs := 0.0;

   Nmin  : constant := 4;
   Nmax  : constant := 4;
   Nstep : constant := 1;
   N     : Integer; -- #Agents

   Tmin  : constant := 20; -- #Tasks
   Tmax  : constant := 20;
   Tstep : constant := 10;
   T     : Integer;

   Corner_Poses : constant Types.Pose_Array (1 .. 4) :=
                    ((0.0,     0.0, Types.Angle (Pi / 4.0)),
                     (0.0,   +Ymax, Types.Angle (-Pi / 4.0)),
                     (+Xmax, +Ymax, Types.Angle (5.0 * Pi / 4.0)),
                     (+Xmax,   0.0, Types.Angle (3.0 * Pi / 4.0)));

   subtype Agent_Name is String (1 .. 3);
   Names  : constant array (1 .. 100) of Agent_Name :=
              ("Ari", "Ben", "Ced", "Dan", others => "Xxx");

   Enable_Draw : constant Boolean := True;

begin
   --  Common configuration.
   Config.Init (Log_Level => Trace.Debug);
   --  Enable_Section (Config.Tracer, Sancta.Traderbot_Sim.Log_Section);

   Reset (Rand);

   Create (Full_Result, Mode => Out_File, Name => "compare1-full.txt");
   Create (Avg_Result,  Mode => Out_File, Name => "compare1-avg.txt");

   for S in Starting_Places'First .. Starting_Places'Last loop -- #Starting poses
      N := Nmin;
      loop -- #Agents
         T := Tmin;
         loop -- #Tasks
            Acum_Optimal := 0.0;
            Acum_Trading := 0.0;

            for I in 1 .. Iterations loop

               New_Line;
               Put_Line ("ITERATION " & S'Img & N'Img & T'Img);

               Agents.Clear;
               Tasks.Clear;

               --  Create agents at starting places lined but sparse:
               declare
                  Sbis : constant Starting_Places := S;
                  --  Workaround to avoid S'range checking below.
               begin
                  case Sbis is
                  when Close =>
                     for I in 0 .. N - 1 loop
                        Bot.Set_Name (Names (I + 1));
                        Bot.Set_Pose ((0.0, Types.Real (I), 0.0));
                        Agents.Append (Bot);
                     end loop;
                  when Lateral =>
                     for I in 0 .. N - 1 loop
                        Bot.Set_Name (Names (I + 1));
                        Bot.Set_Pose ((0.0,
                                       Types.Real (Xmax / Float (N - 1)) * Types.Real (I), 0.0));
                        Agents.Append (Bot);
                     end loop;
                  when Corners =>
                     for I in 1 .. N loop
                        Bot.Set_Name (Names (I));
                        Bot.Set_Pose (Corner_Poses (I));
                        Agents.Append (Bot);
                     end loop;
                  when Random =>
                     for I in 0 .. N - 1 loop
                        Bot.Set_Name (Names (I + 1));
                        Bot.Set_Pose ((Types.Real (Random (Rand) * Xmax),
                                       Types.Real (Random (Rand) * Ymax),
                                       Types.Angle (Random (Rand) * Amax)));
                        Agents.Append (Bot);
                     end loop;
                  end case;
               end;

               --  Create tasks at random places
               Sancta.Tasks.Set_Next_Id (1);
               for I in 1 .. T loop
                  Go.Set_Pose ((Types.Real (Random (Rand) * Xmax),
                                Types.Real (Random (Rand) * Ymax),
                                Types.Angle (Random (Rand) * Amax)));
                  Go.Assign_Id;
                  Tasks.Append (Go);
               end loop;

               --  See how much it costs:
               declare
                  Optimal_Cost : constant Sancta.Costs :=
                                   Sancta.Costs.Get_Optimal_Cost (Agents,
                                                                  Tasks,
                                                                  Draw => I = 1 and Enable_Draw);
                  Trade_Cost   : constant Sancta.Costs :=
                                   Sancta.Costs.Get_Trader_Cost
                                     (Agents,
                                      Tasks,
                                      (First                    => True,
                                       Second                   => True,
                                       All_But_First_And_Second => True,
                                       Generated                => True,
                                       Added                    => False,
                                       Won                      => False),
                                      Traderbot.Best,
                                      Auctioner.Local_Cost,
                                      Draw => I = 1 and Enable_Draw);
               begin
                  Log ("Auctioning cost incurred:   " & Debug.To_String (Trade_Cost),
                       Informative);
                  Log ("Optimal to Suboptimal ratio is " &
                       Debug.To_String (Optimal_Cost / Trade_Cost),
                       Informative);

                  if Trade_Cost < Optimal_Cost - Sancta.Costs (T) then
                     raise Constraint_Error;
                  end if;
                  --  Since there's rounding involved in the TSP transformation,
                  --  a bit of slack is possible I guess...

                  Acum_Optimal := Acum_Optimal + Optimal_Cost;
                  Acum_Trading := Acum_Trading + Trade_Cost;

                  Put_Line (Full_Result,
                            S'Img & " " &
                            N'Img & " " &
                            T'Img & " " &
                            Optimal_Cost'Img &
                            Trade_Cost'Img &
                            Sancta.Costs'Image (Optimal_Cost / Trade_Cost));
                  Flush (Full_Result);
               end;
            end loop; -- Averaging

            Put_Line (Avg_Result,
                      S'Img & " " &
                      N'Img & " " &
                      T'Img & " " &
                      Sancta.Costs'Image (Acum_Optimal / Sancta.Costs (Iterations)) &
                      Sancta.Costs'Image (Acum_Trading /Sancta.Costs (Iterations)) &
                      Sancta.Costs'Image (Acum_Optimal / Acum_Trading));
            Flush (Avg_Result);

            --
            T := T + Tstep;
            exit when T > Tmax;
         end loop; -- Tasks

         N := N + Nstep;
         exit when N > Nmax;
      end loop; -- Agents
   end loop; -- Starting places

   Close (Full_Result);
   Close (Avg_Result);

   Command_Line.Wait_For_Keystroke;

   Agpl.Gdk.Managed.Shutdown;

end Sancta.Main.Compare1draw;
