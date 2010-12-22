 

with Sancta.Agent.Handle;
with Sancta.Assignment;
with Sancta.Tasks.Insertions;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Handle;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers.Indefinite_Ordered_Maps;

package body Sancta.Assigner.Greedy_Minmax_Exhaustive is

   use type Agent.Containers.Lists.Cursor;
   use type Sancta.Tasks.Containers.Lists.Cursor;
   use type Sancta.Tasks.Task_Id;
   use type Sancta.Agent.Handle.Object;
   use type Sancta.Tasks.Handle.Object;

   package Int_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Natural);

   ------------
   -- Assign --
   ------------

   function Assign
     (This   : in Object;
      Agents : in Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List;
      Costs  : in Sancta.Cost_Cache.Object'Class)
      return Assignment.Object
   is
      A : Assignment.Object;
      --  The result we'll return.

      Pending : Sancta.Tasks.Containers.Lists.List := Tasks;
      --  Tasks not yet assigned.

      Not_Before : Int_Maps.Map;
      --  To keep track of untouchable tasks if This.Keep_Order

      -------------------------
      -- Remove_From_Pending --
      -------------------------

      procedure Remove_From_Pending (T : in Sancta.Tasks.Object'Class) is
         use Sancta.Tasks.Containers.Lists;
         I : Cursor := Pending.First;
      begin
         while Has_Element (I) loop
            if Element (I).Get_Id = T.Get_Id then
               Pending.Delete (I);
               return;
            else
               Next (I);
            end if;
         end loop;
         raise Program_Error; -- Shouldn't be reached.
      end Remove_From_Pending;

      ---------------
      -- Try_Agent --
      ---------------
      --  Try all pending tasks in the agent.
      --  Returns a new agent with the task inserted at best place.
      --  Ct holds the new total cost for the modified agent.
      procedure Try_Agent (Ag  : in     Agent.Object'Class;
                           Nw  :    out Agent.Handle.Object;
                           Ct  :    out Sancta.Costs;
                           Job :    out Sancta.Tasks.Handle.Object)
      is
         use Sancta.Tasks.Containers.Lists;
         T  : Cursor  := Pending.First;
      begin
         Ct := Sancta.Costs'Last;
         while Has_Element (T) loop
            declare
               Try_Agent : Agent.Handle.Object;
               Dummy     : Sancta.Costs;
               Try_Total : Sancta.Costs;
               Ok        : Boolean;
            begin
               Sancta.Tasks.Insertions.Greedy
                 (Ag,
                  Element (T),
                  Costs,
                  Int_Maps.Element (Not_Before.Find (Ag.Get_Name)),
                  Try_Agent,
                  Cost_Delta => Dummy,
                  Cost_Total => Try_Total,
                  Success    => Ok);

               if Ok and then Try_Total < Ct then
                  Ct  := Try_Total;
                  Nw  := Try_Agent;
                  Job.Set (Element (T));
               end if;
            end;
            Next (T);
         end loop;
      end Try_Agent;

   begin
      --  Initialize assignment:
      Log ("Starting ass", Always);
      declare
         use Agent.Containers.Lists;
         I : Cursor := Agents.First;
      begin
         while Has_Element (I) loop
            if This.Keep_Order then
               Not_Before.Include (Element (I).Get_Name,
                                   Natural (Element (I).Get_Tasks.Length));
            else
               Not_Before.Include (Element (I).Get_Name,
                                   0);
            end if;

            Log ("Adding agent " & Element (I).Get_Name, Debug, Log_Section);
            A.Set_Agent (Element (I));
            Next (I);
         end loop;
      end;

      --  Assign tasks:
      while not Pending.Is_Empty loop
         Log ("Pending:" & Pending.Length'Img, Debug, Log_Section);
         declare
            Best_Cost        : Sancta.Costs := Sancta.Costs'Last;
            Best_Agent       : Agent.Handle.Object;
            Best_Task        : Sancta.Tasks.Handle.Object;
            use Agent.Containers.Lists;
            I                : Cursor := Agents.First;
         begin
            while Has_Element (I) loop
               declare
                  Mod_Agent : Agent.Handle.Object;
                  Mod_Cost  : Sancta.Costs;
                  Target    : Sancta.Tasks.Handle.Object;
               begin
                  --  Select the best task for a given agent
                  declare
                     Name : constant String := Element (I).Get_Name;
                  begin
                     Try_Agent (A.Get_Agent (Name),
                                Mod_Agent, Mod_Cost, Target);
                  end;

                  if Target.Is_Valid then
                     if Mod_Cost < Best_Cost then
                        Best_Cost  := Mod_Cost;
                        Best_Agent.Set (Mod_Agent.Get);
                        Best_Task.Set (Target.Get);
                     end if;
                  end if;
               end;
               Next (I);
            end loop;

            if Best_Agent.Is_Valid then
               A.Set_Agent (Best_Agent.Get);
               Remove_From_Pending (Best_Task.Get);
            else
               A.Set_Valid (False);
               return A;
            end if;
         end;
      end loop;

      A.Set_Valid;

      return A;
   end Assign;

end Sancta.Assigner.Greedy_Minmax_Exhaustive;
