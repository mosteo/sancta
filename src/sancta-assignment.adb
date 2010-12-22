

with Agpl.Conversions;
--  with Sancta.Agent.Handle;
with Sancta.Plan.Utils;
with Sancta.Tasks.Extra;
with Sancta.Tasks.Utils;
with Agpl; use Agpl;

package body Sancta.Assignment is

   package Agent_Lists renames Sancta.Agent.Containers.Lists;
   package Ac renames Sancta.Agent.Containers;
   package Task_Lists renames Sancta.Tasks.Containers.Lists;
   package Tc renames Sancta.Tasks.Containers;
   function To_String is new Conversions.Decimal_To_Str (Costs);

--------------
-- Is_Empty --
--------------

   function Is_Empty (This : Object) return Boolean is
   begin
      return This.Get_All_Tasks.Is_Empty;
   end Is_Empty;

   ---------
   -- Add --
   ---------

   procedure Add
     (This     : in out Object;
      Agent    : in     Sancta.Agent.Object'Class;
      The_Task : in     Sancta.Tasks.Object'Class)
   is
      use Sancta.Agent.Containers.Maps;
   begin
      if Contains (This.Agents, Sancta.Agent.Get_Name (Agent)) then
         declare
            A : Sancta.Agent.Object'Class :=
                  Element (Find (This.Agents, Sancta.Agent.Get_Name (Agent)));
         begin
            A.Add_Task (The_Task);
            This.Agents.Replace (A.Get_Name, A);
         end;
      else
         declare
            A : Sancta.Agent.Object'Class := Agent;
         begin
            A.Clear_Tasks;
            A.Add_Task (The_Task);
            This.Agents.Insert (A.Get_Name, A);
         end;
      end if;
   end Add;

   procedure Prepend
     (This     : in out Object;
      Agent    : in     Sancta.Agent.Object'Class;
      The_Task : in     Sancta.Tasks.Object'Class)
   is
   begin
      if not This.Contains (Agent.Get_Name) then
         This.Add (Agent, The_Task);
      elsif This.Get_Tasks (Agent).Is_Empty then
         This.Add (Agent, The_Task);
      else
         declare
            L : Tc.Lists.List := This.Get_Tasks (Agent);
         begin
            L.Prepend (The_Task);
            This.Set_Tasks (Agent.Get_Name, L);
         end;
      end if;
   end Prepend;

   ---------
   -- Add --
   ---------

   procedure Add
     (This      : in out Object;
      Agent     : in     Sancta.Agent.Object'Class;
      The_Tasks : in     Sancta.Tasks.Containers.Lists.List)
   is
      use Sancta.Agent.Containers.Maps;
   begin
      if Contains (This.Agents, Agent.Get_Name) then
         declare
            A : Sancta.Agent.Object'Class :=
                  Element (Find (This.Agents, Sancta.Agent.Get_Name (Agent)));
         begin
            A.Add_Tasks (The_Tasks);
            This.Agents.Replace (A.Get_Name, A);
         end;
      else
         declare
            A : Sancta.Agent.Object'Class := Agent;
         begin
            A.Clear_Tasks;
            A.Add_Tasks (The_Tasks);
            This.Agents.Insert (A.Get_Name, A);
         end;
      end if;
   end Add;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This  : in out Object;
                        Agent :        String;
                        Jobs  :        Sancta.Tasks.Containers.Lists.List)
   is
      A : Sancta.Agent.Object'Class := This.Get_Agent (Agent);
   begin
      A.Set_Tasks (Jobs);
      This.Set_Agent (A);
   end Set_Tasks;

   --------------
   -- Set_Task --
   --------------

   procedure Set_Task (This  : in out Object;
                       Agent :        String;
                       Job   :        Sancta.Tasks.Object'Class)
   is
      A : Sancta.Agent.Object'Class := This.Get_Agent (Agent);
   begin
      A.Set_Task (Job);
      This.Set_Agent (A);
   end Set_Task;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Object) is
   begin
      This.Agents.Clear;
      This.Ok := True;
   end Clear;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This : in out Object) is
      procedure Check (I : Ac.Maps.Cursor) is
         procedure Process (Name : String; A : in out Sancta.Agent.Object'Class) is
            pragma Unreferenced (Name);
         begin
            A.Clear_Tasks;
         end Process;
      begin
         This.Agents.Update_Element (I, Process'Access);
      end Check;
   begin
      This.Agents.Iterate (Check'Access);
   end Clear_Tasks;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This  : in out Object;
                          Agent :        String)
   is
   begin
      if This.Contains (Agent) then
         This.Set_Tasks (Agent, Tc.Lists.Empty_List);
      end if;
   end Clear_Tasks;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object; Name : in String) return Boolean is
   begin
      return This.Agents.Contains (Name);
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object; Id : in Sancta.Tasks.Task_Id)
                      return Boolean
   is
      use Agent.Containers.Maps;
      I : Cursor := This.Agents.First;
   begin
      while Has_Element (I) loop
         if Sancta.Tasks.Extra.Contains (Element (I).Get_Tasks, Id) then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

   ------------------
   -- Empty_Object --
   ------------------

   function Empty_Object return Object'Class is
   begin
      return Object'(Agents => Sancta.Agent.Containers.Maps.Empty_Map,
                     Ok     => True);
   end Empty_Object;

   ------------------
   -- Empty_Object --
   ------------------

   function Empty_Object return Object is
   begin
      return Object'(Agents => Sancta.Agent.Containers.Maps.Empty_Map,
                     Ok     => True);
   end Empty_Object;

   ------------
   -- Create --
   ------------

   function Create (Agent : Sancta.Agent.Object'Class) return Object is
      This : Object;
   begin
      This.Agents.Insert (Agent.Get_Name, Agent);
      return This;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Agents : Sancta.Agent.Containers.Lists.List)
                    return Object is
      This : Object;
   begin
      This.Set_Agents (Agents);
      return This;
   end Create;

   -----------------
   -- Fill_Owners --
   -----------------

   procedure Fill_Owners (This : in Object; Plan : in out Sancta.Plan.Object) is

      --------------------
      -- For_Each_Agent --
      --------------------

      procedure For_Each_Agent (I : Agent.Containers.Maps.Cursor) is

         -------------------
         -- For_Each_Task --
         -------------------

         procedure For_Each_Task (J : Sancta.Tasks.Containers.Lists.Cursor) is
         begin
            Sancta.Plan.Set_Task_Owner
              (Plan,
               Sancta.Tasks.Get_Id (Sancta.Tasks.Containers.Lists.Element (J)),
               Agent.Get_Name (Agent.Containers.Maps.Element (I)));
         end For_Each_Task;

         T : constant Sancta.Tasks.Containers.Lists.List := Agent.Get_Tasks (Agent.Containers.Maps.Element (I));
      begin
         T.Iterate (For_Each_Task'Access);
      end For_Each_Agent;
   begin
      Agent.Containers.Maps.Iterate (This.Agents, For_Each_Agent'Access);
   end Fill_Owners;

   ---------------
   -- Get_Agent --
   ---------------

   function Get_Agent (This : in Object; Name : in String)
                       return Agent.Object'Class
   is
      use Agent.Containers.Maps;
   begin
      return Element (Find (This.Agents, Name));
   end Get_Agent;

   ---------------
   -- Get_Agent --
   ---------------

   function Get_Agent (This : Object; Id : Sancta.Tasks.Task_Id)
                       return Agent.Object'Class
   is
      use Agent.Containers.Maps;
      I : Cursor := This.Agents.First;
   begin
      while Has_Element (I) loop
         if Sancta.Tasks.Extra.Contains (Element (I).Get_Tasks, Id) then
            return Element (I);
         end if;
         Next (I);
      end loop;
      raise Constraint_Error with "No agent contains given task";
   end Get_Agent;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents (This : Object; Id : Sancta.Tasks.Task_Id)
                        return Ac.Lists.List
   is
      use Agent.Containers.Maps;
      I : Cursor := This.Agents.First;
      R : Ac.Lists.List;
   begin
      while Has_Element (I) loop
         if Sancta.Tasks.Extra.Contains (Element (I).Get_Tasks, Id) then
            R.Append (Element (I));
         end if;
         Next (I);
      end loop;

      return R;
   end Get_Agents;

   ------------------
   -- Remove_Agent --
   ------------------

   procedure Remove_Agent (This : in out Object; Name : String) is
   begin
      This.Agents.Delete (Name);
   end Remove_Agent;

   -------------------
   -- Remove_Agents --
   -------------------

   procedure Remove_Agents (This  : in out Object;
                            Names :        Agent.Containers.Lists.List)
   is
      procedure Do_It (I : Ac.Lists.Cursor) is
      begin
         This.Agents.Delete (Ac.Lists.Element (I).Get_Name);
      end Do_It;
   begin
      Names.Iterate (Do_It'Access);
   end Remove_Agents;

   ---------------
   -- Set_Agent --
   ---------------

   procedure Set_Agent (This : in out Object; Agent : in Sancta.Agent.Object'Class) is
   begin
      This.Agents.Include (Agent.Get_Name, Agent);
   end Set_Agent;

   ----------------
   -- Set_Agents --
   ----------------

   procedure Set_Agents (This   : in out Object;
                         Agents :        Sancta.Agent.Containers.Lists.List)
   is
      use Sancta.Agent.Containers.Lists;
      procedure Add (I : Cursor) is
      begin
         This.Set_Agent (Element (I));
      end Add;
   begin
      Agents.Iterate (Add'Access);
   end Set_Agents;

   -------------------------
   -- Fill_Missing_Agents --
   -------------------------

   procedure Fill_Missing_Agents
     (This   : in out Object;
      Agents :        Sancta.Agent.Containers.Lists.List)
   is
      Prev : constant Sancta.Agent.Containers.Lists.List := This.Get_Agents;
   begin
      This.Set_Agents (Agents);
      This.Set_Agents (Prev);
   end Fill_Missing_Agents;

   -----------------
   -- Freeze_Plan --
   -----------------

   function Freeze_Plan (This : in Object;
                         P    : in Sancta.Plan.Object)
                         return    Sancta.Plan.Object
   is
      Result : Sancta.Plan.Object := P;

      procedure Check (I : in Agent.Containers.Maps.Cursor) is
         TL : constant Sancta.Tasks.Containers.Lists.List := Agent.Containers.Maps.Element (I).Get_Tasks;

         procedure Check_Tasks (T : in Sancta.Tasks.Containers.Lists.Cursor) is
         begin
            Sancta.Plan.Utils.Trim_Or_Siblings
              (Result, Sancta.Tasks.Containers.Lists.Element (T).Get_Id);
         end Check_Tasks;
      begin
         Tl.Iterate (Check_Tasks'Access);
      end Check;
   begin
--        Log ("About to freeze plan: ", Always);
--        Result.Print_Tree_Summary;
--        Log ("With assignment: ", Always);
--        This.Print_Assignment;
      This.Agents.Iterate (Check'Access);
      return Result;
   end Freeze_Plan;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents (This : in Object)
                        return Agent.Containers.Lists.List
   is
      use Agent.Containers.Lists;
      use Agent.Containers.Maps;

      Result : Agent.Containers.Lists.List;

      procedure Add (X : in Agent.Containers.Maps.Cursor) is
      begin
         Append (Result, Element (X));
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents (This  : Object;
                        Which : Agpl.Containers.String_Sets.Set)
                        return Object'Class
   is
      use Agent.Containers.Lists;
      use Agent.Containers.Maps;

      Result : Agent.Containers.Lists.List;

      procedure Add (X : in Agent.Containers.Maps.Cursor) is
      begin
         if Which.Contains (Key (X)) then
            Append (Result, Element (X));
         end if;
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Create (Result);
   end Get_Agents;

   ------------------------------
   -- Get_Agents_Without_Tasks --
   ------------------------------

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Lists.List
   is
      use Agent.Containers.Lists;
      use Agent.Containers.Maps;

      Result : Agent.Containers.Lists.List;

      procedure Add (X : in Agent.Containers.Maps.Cursor) is
         A : Sancta.Agent.Object'Class := Element (X);
      begin
         A.Clear_Tasks;
         Append (Result, A);
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents_Without_Tasks;

   ------------------------------
   -- Get_Agents_Without_Tasks --
   ------------------------------

   function Get_Agents_Without_Tasks (This : in Object)
                                      return    Agent.Containers.Vectors.Vector
   is
      use Agent.Containers.Vectors;
      use Agent.Containers.Maps;

      Result : Agent.Containers.Vectors.Vector;

      procedure Add (X : in Agent.Containers.Maps.Cursor) is
         A : Sancta.Agent.Object'Class := Element (X);
      begin
         A.Clear_Tasks;
         Append (Result, A);
      end Add;

   begin
      Iterate (This.Agents, Add'Access);

      return Result;
   end Get_Agents_Without_Tasks;

   ---------------------------
   -- Get_Less_Costly_Agent --
   ---------------------------

   function Get_Less_Costly_Agent (This : in Object) return Agent.Object'Class
   is
      use Agent.Containers.Maps;

      Best   : Cursor   := No_Element;
      I      : Cursor   := This.Agents.First;
      Cost   : Sancta.Costs := Sancta.Infinite;
   begin
      while Has_Element (I) loop
         declare
            This_Cost : constant Sancta.Costs := Element (I).Get_Plan_Cost;
         begin
            if This_Cost < Cost then
               Cost := This_Cost;
               Best := I;
            end if;
         end;
         Next (I);
      end loop;

      if Has_Element (Best) then
         return Element (Best);
      else
         raise Constraint_Error with "No agents with finite cost";
      end if;
   end Get_Less_Costly_Agent;

   ------------------------------------
   -- Get_Less_Costly_Non_Idle_Agent --
   ------------------------------------

   function Get_Less_Costly_Non_Idle_Agent (This : in Object)
                                            return Agent.Object'Class
   is
      Aux : Object;
   begin
      Aux.Set_Agents (This.Get_Non_Idle_Agents);
      return Aux.Get_Less_Costly_Agent;
   end Get_Less_Costly_Non_Idle_Agent;

   ---------------------------
   -- Get_Most_Costly_Agent --
   ---------------------------

   function Get_Most_Costly_Agent (This : in Object) return Agent.Object'Class
   is
      use Agent.Containers.Maps;

      Best   : Cursor   := No_Element;
      I      : Cursor   := This.Agents.First;
      Cost   : Sancta.Costs := 0.0;
   begin
      while Has_Element (I) loop
         declare
            This_Cost : constant Sancta.Costs := Element (I).Get_Plan_Cost;
         begin
            if This_Cost >= Cost then
               Cost := This_Cost;
               Best := I;
            end if;
         end;
         Next (I);
      end loop;

      if Has_Element (Best) then
         return Element (Best);
      else
         raise Constraint_Error with "No agents";
      end if;
   end Get_Most_Costly_Agent;

   -------------------
   -- Get_All_Tasks --
   -------------------

   function Get_All_Tasks (This : in Object) return Sancta.Tasks.Containers.Lists.List is
      Result : Sancta.Tasks.Containers.Lists.List;
      use Sancta.Agent.Containers.Maps;
      I      : Cursor := This.Agents.First;
   begin
      while Has_Element (I) loop
         Sancta.Tasks.Utils.Concatenate (Result, Sancta.Agent.Get_Tasks (Element (I)));
         Next (I);
      end loop;

      return Result;
   end Get_All_Tasks;

   -------------------------
   -- Get_All_First_Tasks --
   -------------------------

   function Get_All_First_Tasks (This : in Object)
                                 return Sancta.Tasks.Containers.Lists.List
   is
      Result : Sancta.Tasks.Containers.Lists.List;
      use Sancta.Agent.Containers.Maps;
      I      : Cursor := This.Agents.First;
   begin
      while Has_Element (I) loop
         if Element (I).Has_Tasks then
            Result.Append (Element (I).Get_First_Task);
         end if;
         Next (I);
      end loop;

      return Result;
   end Get_All_First_Tasks;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks
     (This : in Object;
      Agent : in Sancta.Agent.Object'Class)
      return Sancta.Tasks.Containers.Lists.List
   is
   begin
      return This.Get_Tasks (Agent.Get_Name);
   end Get_Tasks;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This  : in Object;
                       Agent : in String)
                       return Sancta.Tasks.Containers.Lists.List
   is
      use Sancta.Agent.Containers.Maps;
      Empty : Sancta.Tasks.Containers.Lists.List;
   begin
      if This.Agents.Contains (Agent) then
         return This.Agents.Element (Agent).Get_Tasks;
      else
         return Empty;
      end if;
   end Get_Tasks;

   ----------------------
   -- Get_Max_Min_Cost --
   ----------------------

   function Get_Max_Min_Cost (This : in Object) return Costs is
      Worst : Costs := 0.0;
      use Agent.Containers.Maps;
      I : Cursor := First (This.Agents);
   begin
      if not This.Ok then
         return Infinite;
      end if;

      while I /= No_Element loop
         Worst := Costs'Max
           (Worst,
            Agent.Get_Plan_Cost (Element (I)));
         Next (I);
      end loop;

      return Worst;
   end Get_Max_Min_Cost;

   function Get_Max_Min_Cost (This : in Object;
                              C    : in Cost_Cache.Object'Class) return Costs
   is
      Worst : Costs := 0.0;
      use Agent.Containers.Maps;
      I : Cursor := First (This.Agents);
   begin
      if not This.Ok then
         raise Constraint_Error with "Invalid ass";
         --  Above is just for checking with NERUS; remove when done
         return Infinite;
      end if;

      while I /= No_Element loop
         Worst := Costs'Max
           (Worst,
            Cost_Cache.Get_Plan_Cost (C, Element (I)));
         Next (I);
      end loop;

      return Worst;
   end Get_Max_Min_Cost;

   --------------------------
   -- Get_Cummulative_Cost --
   --------------------------

   function Get_Cummulative_Cost (This : in Object) return Costs is
      Cost : Costs := 0.0;
      use Agent.Containers.Maps;
      I : Cursor := First (This.Agents);
   begin
      if not This.Ok then
         return Infinite;
      end if;

      while I /= No_Element loop
         Cost := Cost + Agent.Get_Plan_Cost (Element (I));
         Next (I);
      end loop;

      return Cost;
   end Get_Cummulative_Cost;

   function Get_Cummulative_Cost (This : in Object;
                                  C    : in Cost_Cache.Object'Class) return Costs
   is
      Cost : Costs := 0.0;
      use Agent.Containers.Maps;
      I : Cursor := First (This.Agents);
   begin
      if not This.Ok then
         return Infinite;
      end if;

      while I /= No_Element loop
         Cost := Cost + Cost_Cache.Get_Plan_Cost (C, Element (I));
         Next (I);
      end loop;

      return Cost;
   end Get_Cummulative_Cost;

   ----------------------
   -- Get_Average_Cost --
   ----------------------

   function Get_Average_Cost (This : Object;
                              Div  : Boolean := True) return Costs
   is
      Total_Cost  : Costs   := 0.0;
      Total_Tasks : Natural := 0;

      procedure Check (I : Ac.Maps.Cursor) is
         procedure Process (K : String; Agent : Sancta.Agent.Object'Class) is
            pragma Unreferenced (K);
            Agent_Cost : Costs := 0.0;

            procedure Check_Tasks (Tasks : Tc.Lists.List) is
               procedure Check_Task (I : Tc.Lists.Cursor) is
                  Prev_Task : constant Tc.Lists.Cursor := Tc.Lists.Previous (I);
                  procedure Check_Id (T : Sancta.Tasks.Object'Class) is
                     use Tc.Lists;
                  begin
                     if not Has_Element (Prev_Task) then
                        Agent_Cost := Agent_Cost + Agent.Get_Cost (T);
                     else
                        Agent_Cost := Agent_Cost +
                          Agent.Get_Cost (Element (Prev_Task), T);
                     end if;
                     Total_Cost := Total_Cost + Agent_Cost;
                  end Check_Id;
               begin
                  Total_Tasks := Total_Tasks + 1;
                  Tc.Lists.Query_Element (I, Check_Id'Access);
               end Check_Task;
            begin
               Tasks.Iterate (Check_Task'Access);
            end Check_Tasks;
         begin
            Agent.Check_Task_List (Check_Tasks'Access);
         end Process;
      begin
         Ac.Maps.Query_Element (I, Process'Access);
      end Check;
   begin
      This.Agents.Iterate (Check'Access);

      if Total_Tasks = 0 then
         return 0.0;
      elsif Div then
         return Total_Cost / Total_Tasks;
      else
         return Total_Cost;
      end if;
   end Get_Average_Cost;

   ----------------------
   -- Get_Average_Cost --
   ----------------------

   function Get_Average_Cost (This : Object;
                              C    : Cost_Cache.Object'Class;
                              Div  : Boolean := True) return Costs
   is
      Total_Cost  : Costs   := 0.0;
      Total_Tasks : Natural := 0;

      procedure Check (I : Ac.Maps.Cursor) is
         procedure Process (K : String; Agent : Sancta.Agent.Object'Class) is
            pragma Unreferenced (K);
            Agent_Cost : Costs := 0.0;

            procedure Check_Tasks (Tasks : Tc.Lists.List) is
               Prev_Task : Sancta.Tasks.Task_Id := Sancta.Tasks.No_Task;

               procedure Check_Task (I : Tc.Lists.Cursor) is
                  procedure Check_Id (T : Sancta.Tasks.Object'Class) is
                  begin
                     Agent_Cost := Agent_Cost +
                       C.Get_Cost (Agent.Get_Name, Prev_Task, T.Get_Id);
                     Total_Cost := Total_Cost + Agent_Cost;
                     Prev_Task  := T.Get_Id;
                  end Check_Id;
               begin
                  Total_Tasks := Total_Tasks + 1;
                  Tc.Lists.Query_Element (I, Check_Id'Access);
               end Check_Task;
            begin
               Tasks.Iterate (Check_Task'Access);
            end Check_Tasks;
         begin
            Agent.Check_Task_List (Check_Tasks'Access);
         end Process;
      begin
         Ac.Maps.Query_Element (I, Process'Access);
      end Check;
   begin
      This.Agents.Iterate (Check'Access);

      if Total_Tasks = 0 then
         return 0.0;
      elsif Div then
         return Total_Cost / Total_Tasks;
      else
         return Total_Cost;
      end if;
   end Get_Average_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (This      : in Object;
                      Criterion : in Assignment_Criteria) return Costs
   is
      Mm : Sancta.Costs := 0.0;
      Ms : Sancta.Costs := 0.0;
      Ma : Sancta.Costs := 0.0;
   begin
      if This.Ok then
         if Criterion.Minmax_Weight /= 0.0 then
            Mm := This.Get_Max_Min_Cost;
         end if;
         if Criterion.Minsum_Weight /= 0.0 then
            Ms := This.Get_Cummulative_Cost;
         end if;
         if Criterion.Minavg_Weight /= 0.0 then
            Ma := This.Get_Average_Cost;
         end if;
      end if;

      if This.Ok then
         return Evaluate (Criterion,
                          Minmax => Mm,
                          Minsum => Ms,
                          Minavg => Ma);
      else
         return Infinite;
      end if;
   end Get_Cost;

   function Get_Cost (This      : in Object;
                      C         : in Cost_Cache.Object'Class;
                      Criterion : in Assignment_Criteria) return Costs
   is
      Mm : Sancta.Costs := 0.0;
      Ms : Sancta.Costs := 0.0;
      Ma : Sancta.Costs := 0.0;
   begin
      if This.Ok then
         if Criterion.Minmax_Weight /= 0.0 then
            Mm := This.Get_Max_Min_Cost (C);
         end if;
         if Criterion.Minsum_Weight /= 0.0 then
            Ms := This.Get_Cummulative_Cost (C);
         end if;
         if Criterion.Minavg_Weight /= 0.0 then
            Ma := This.Get_Average_Cost (C);
         end if;
      end if;

      if This.Ok then
         return Evaluate (Criterion,
                          Minmax => Mm,
                          Minsum => Ms,
                          Minavg => Ma);
      else
         return Infinite;
      end if;
   end Get_Cost;

   -------------------------
   -- Get_Cost_Until_Task --
   -------------------------

   function Get_Cost_Until_Task (This      : in Object;
                                 Job       : in Sancta.Tasks.Task_Id;
                                 Criterion : in Assignment_Criteria)
                                 return    Sancta.Costs
   is
      Agents : constant Agent_Lists.List := This.Get_Agents;
      Minmax,
      Minsum : Sancta.Costs := 0.0;

      -----------------
      -- Check_Agent --
      -----------------

      procedure Check_Agent (I : Agent_Lists.Cursor) is
         Tasks : constant Task_Lists.List := Agent_Lists.Element (I).Get_Tasks;
         Acum  : Sancta.Costs := 0.0;
         T     : Task_Lists.Cursor := Tasks.First;
         use Agent_Lists;
         use Task_Lists;
         use type Sancta.Tasks.Task_Id;
      begin
         while Task_Lists.Has_Element (T) loop
            if T /= Tasks.First then
               Acum := Acum + Element (I).Get_Cost (Element (Previous (T)),
                                                    Element (T));
            else
               Acum := Acum + Element (I).Get_Cost (Element (T));
            end if;

            if Element (T).Get_Id = Job then
               Minmax := Acum;
               exit;
            end if;
            Task_Lists.Next (T);
         end loop;
      end Check_Agent;

      procedure Check_Minsum (I : Agent_Lists.Cursor) is
         use Agent_Lists;
      begin
         Minsum := Minsum + Sancta.Costs'Min (Minmax, Element (I).Get_Plan_Cost);
      end Check_Minsum;

   begin
      if not This.Ok then
         return Infinite;
      end if;

      Agents.Iterate (Check_Agent'Access);
      Agents.Iterate (Check_Minsum'Access);
      return Criteria.Evaluate (Criterion,
                          Minmax => Minmax,
                          Minsum => Minsum,
                          Minavg => 0.0);
   end Get_Cost_Until_Task;

   ------------------------
   -- Invalid_Assignment --
   ------------------------

   function Invalid_Assignment return Object'Class is
      This : Object := (others => <>);
   begin
      This.Ok := False;
      return This;
   end Invalid_Assignment;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return This.Ok;
   end Is_Valid;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid (This : in out Object; Valid : in Boolean := True) is
   begin
      This.Ok := Valid;
   end Set_Valid;

   ----------------------
   -- Print_Assignment --
   ----------------------

   procedure Print_Assignment (This : in Object) is
   begin
      Log ("** Assignment detail **", Always);

      if not Is_Valid (This) then
         Log ("Invalid assignment!", Always);
      end if;

      declare
         use Sancta.Agent.Containers.Maps;
         I : Cursor := First (This.Agents);
      begin
         while Has_Element (I) loop

            Log ("** Agent : " & Key (I), Always);

            declare
               T : constant Sancta.Tasks.Containers.Lists.List := Element (I).Get_Tasks;
               J : Sancta.Tasks.Containers.Lists.Cursor        := T.First;
               use Sancta.Tasks.Containers.Lists;
            begin
               if not Has_Element (J) then
                  Log ("No tasks", Always);
               end if;

               while Has_Element (J) loop
                  Log (Element (J).Get_Id'Img & "-" & Element (J).To_String,
                       Always);
                  Next (J);
               end loop;
            end;
            Next (I);
            Log ("", Always);
         end loop;

         Log ("MinSum cost: " & To_String (This.Get_Cummulative_Cost), Always);
         Log ("MinMax cost: " & To_String (This.Get_Max_Min_Cost), Always);
         Log ("", Always);
      end;
   end Print_Assignment;

   -------------------
   -- Print_Summary --
   -------------------

   procedure Print_Summary (This : in Object) is
   begin
      Log ("** Assignment summary **", Always);
      Log ("MinSum cost: " & To_String (This.Get_Cummulative_Cost), Always);
      Log ("MinMax cost: " & To_String (This.Get_Max_Min_Cost), Always);
      Log ("", Always);
   end Print_Summary;

   ---------------------
   -- Get_Idle_Agents --
   ---------------------

   function Get_Idle_Agents (This : Object)
                             return Sancta.Agent.Containers.Lists.List
   is
      Idles : Ac.Lists.List;
      procedure Check (I : Ac.Lists.Cursor) is
         A : constant Sancta.Agent.Object'Class :=
           Sancta.Agent.Object'Class (Ac.Lists.Element (I));
      begin
         if not A.Has_Tasks then
            Idles.Append (A);
         end if;
      end Check;
   begin
      This.Get_Agents.Iterate (Check'Access);
      return Idles;
   end Get_Idle_Agents;

   -------------------------
   -- Get_Non_Idle_Agents --
   -------------------------

   function Get_Non_Idle_Agents (This : Object)
                                 return Sancta.Agent.Containers.Lists.List
   is
      Non_Idles : Ac.Lists.List;
      procedure Check (I : Ac.Lists.Cursor) is
         A : constant Sancta.Agent.Object'Class :=
           Sancta.Agent.Object'Class (Ac.Lists.Element (I));
      begin
         if A.Has_Tasks then
            Non_Idles.Append (A);
         end if;
      end Check;
   begin
      This.Get_Agents.Iterate (Check'Access);
      return Non_Idles;
   end Get_Non_Idle_Agents;

   ----------------
   -- Copy_Tasks --
   ----------------

   procedure Copy_Tasks (Dst : in out Object;
                         Src :        Object)
   is
      Agents : constant Ac.Lists.List := Src.Get_Agents;
      procedure Check (I : Ac.Lists.Cursor) is
         Agent : constant Sancta.Agent.Object'Class := Ac.Lists.Element (I);
      begin
         if not Dst.Contains (Agent.Get_Name) then
            Dst.Set_Agent (Agent);
         else
            Dst.Set_Tasks (Agent.Get_Name, Agent.Get_Tasks);
         end if;
      end Check;
   begin
      Agents.Iterate (Check'Access);
   end Copy_Tasks;

   --------------------------
   -- Merge_Missing_Robots --
   --------------------------

   procedure Merge_Missing_Robots (Dst        : in out Sancta.Assignment.Object;
                                   Src        :        Sancta.Assignment.Object;
                                   With_Tasks :        Boolean := True)
   is
      Agents : Ac.Lists.List;
      procedure Check (I : Ac.Lists.Cursor) is
         Agent : constant Sancta.Agent.Object'Class := Ac.Lists.Element (I);
      begin
         if not Dst.Contains (Agent.Get_Name) then
            Dst.Set_Agent (Agent);
         end if;
      end Check;
   begin
      if With_Tasks then
         Agents := Src.Get_Agents;
      else
         Agents := Src.Get_Agents_Without_Tasks;
      end if;
      Agents.Iterate (Check'Access);
   end Merge_Missing_Robots;

   ---------------
   -- Dummy_Key --
   ---------------

   function Dummy_Key (This : Object) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Dummy_Key;

   -------------
   -- Process --
   -------------

   procedure Process (This : in out Object;
                      What : access procedure (A : in out Agent.Object'Class))
   is
      use Ac.Maps;
      procedure Query (I : Cursor) is
         procedure Update (K : String; A : in out Agent.Object'Class) is
            pragma Unreferenced (K);
         begin
            What (A);
         end Update;
      begin
         This.Agents.Update_Element (I, Update'Access);
      end Query;
   begin
      This.Agents.Iterate (Query'Access);
   end Process;

end Sancta.Assignment;
