with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Agent.Dummy;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;

package body Sancta.Cost_Matrix is

   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;
   package AC renames Sancta.Agent.Containers;
   package TC renames Sancta.Tasks.Containers;
   package Al renames Sancta.Agent.Containers.Lists;
   package Tl renames Sancta.Tasks.Containers.Lists;

   ---------
   -- Key --
   ---------

   function Key (Agent : in String;
                 Ini   : in Sancta.Tasks.Task_Id;
                 Fin   : in Sancta.Tasks.Task_Id) return String
   is
   begin
      return Agent & ":" & Ini'Img & ":" & Fin'Img;
   end Key;

   --------------
   -- Contains --
   --------------

   function Contains
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean
   is
   begin
      return This.Matrix.Contains (Key (Agent, Ini, Fin));
   end Contains;

   ------------
   -- Create --
   ------------

   procedure Create
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
      use type AL.Cursor; use type TL.Cursor;

      A   : AL.Cursor := AL.First (Agents);
      Ini,
      Fin : TL.Cursor;
   begin
      while A /= AL.No_Element loop
         declare
            Bot   : constant Sancta.Agent.Object'Class := Al.Element (A);
            Name  : constant String            := Bot.Get_Name;
         begin
            Ini := Tl.First (Tasks);
            while Ini /= Tl.No_Element loop

               Fin := Tl.First (Tasks);
               while Fin /= Tl.No_Element loop

                  if Ini /= Fin then
                     declare
                        Tini  : constant Sancta.Tasks.Object'Class := Tl.Element (Ini);
                        Tfin  : constant Sancta.Tasks.Object'Class := Tl.Element (Fin);
                        Iniid : constant Sancta.Tasks.Task_Id := Tini.Get_Id;
                        Finid : constant Sancta.Tasks.Task_Id := Tfin.Get_Id;
                     begin
                        Set_Cost (This, Name, Iniid, Finid, Bot.Get_Cost (Tini, Tfin));
                        Set_Cost (This, Name, Finid, Iniid, Bot.Get_Cost (Tfin, Tini));
                     end;
                  else
                     --  Same task... may be should be Infinite, maybe zero?
                     pragma Ummmm;
                     null;
                  end if;

                  Tl.Next (Fin);
               end loop;
               Set_Cost (This, Name,
                         Sancta.Tasks.No_Task,
                         Tl.Element (Ini).Get_Id,
                         Bot.Get_Cost (Tl.Element (Ini)));
               Tl.Next (Ini);
            end loop;
         end;
         AL.Next (A);
      end loop;

      --  Add the No_Task specials
      A := Agents.First;
      while Al.Has_Element (A) loop
         Ini := Tasks.First;
         while Tl.Has_Element (Ini) loop

            Set_Cost (This,
                      Sancta.Agent.Get_Name (Al.Element (A)),
                      Sancta.Tasks.Get_Id (Tl.Element (Ini)),
                      Sancta.Tasks.No_Task,
                      0.0);

            Tl.Next (Ini);
         end loop;
         Al.Next (A);
      end loop;

   end Create;

   -------------------------
   -- Create_Closed_Costs --
   -------------------------

   procedure Create_Closed_Costs
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
      A   : Al.Cursor;
      Ini : Tl.Cursor;
   begin
      A := Agents.First;
      while Al.Has_Element (A) loop
         Ini := Tasks.First;
         while Tl.Has_Element (Ini) loop
            Set_Cost
              (This,
               Sancta.Agent.Get_Name (Al.Element (A)),
               Sancta.Tasks.Get_Id (Tl.Element (Ini)),
               Sancta.Tasks.No_Task,
               This.Get_Cost
                 (Al.Element (A).Get_Name,
                  Sancta.Tasks.No_Task,
                  Sancta.Tasks.Get_Id (Tl.Element (Ini))));
            Tl.Next (Ini);
         end loop;
         Al.Next (A);
      end loop;
   end Create_Closed_Costs;

   ------------
   -- Create --
   ------------

   procedure Create
     (This   : in out Object;
      Agent  : in Sancta.Agent.Object'Class;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
      A : Sancta.Agent.Containers.Lists.List;
   begin
      A.Append (Agent);
      Create (This, A, Tasks);
   end Create;

   -----------------------
   -- Create_With_Start --
   -----------------------

   function Create_With_Start
     (Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List) return Object
   is
      Result : Object;
   begin
      Create_With_Start (Result, Agents, Tasks);
      return Result;
   end Create_With_Start;

   -----------------------
   -- Create_With_Start --
   -----------------------

   procedure Create_With_Start
     (This   : in out Object;
      Agent  : in Sancta.Agent.Object'Class;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
   A : Sancta.Agent.Containers.Lists.List;
   begin
      A.Append (Agent);
      Create_With_Start (This, A, Tasks);
   end Create_With_Start;

   -----------------------
   -- Create_With_Start --
   -----------------------

   procedure Create_With_Start
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
   begin
      Create (This, Agents, Tasks);
   end Create_With_Start;

   -----------------------
   -- Create_Only_Start --
   -----------------------

   procedure Create_Only_Start
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
      A : AC.Lists.Cursor := Agents.First;
      T : TC.Lists.Cursor;
   begin
      while AC.Lists.Has_Element (A) loop
         T := Tasks.First;
         while TC.Lists.Has_Element (T) loop
            Set_Cost (This,
                      Sancta.Agent.Get_Name (AC.Lists.Element (A)),
                      Sancta.Tasks.No_Task,
                      Sancta.Tasks.Get_Id (TC.Lists.Element (T)),
                      AC.Lists.Element (A).Get_Cost (TC.Lists.Element (T)));
            TC.Lists.Next (T);
         end loop;
         AC.Lists.Next (A);
      end loop;
   end Create_Only_Start;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Object;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id)
      return Costs
   is
      I : constant Cursor := Find (This.Matrix, Key (Agent, Ini, Fin));
   begin
      if I = No_Element then
         if Fin = Sancta.Tasks.No_Task then
            return 0.0;
         else
            --  This.Print;
            --  raise Constraint_Error with "Missing cost: " & Key (Agent, Ini, Fin);
            Log ("Missing cost: " & Key (Agent, Ini, Fin),
                 Warning, Log_Section);
            raise Program_Error;
            return Infinite;
         end if;
      else
         return Element (I);
      end if;
   end Get_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in Sancta.Agent.Object'Class) return Costs
   is
      T    : constant Sancta.Tasks.Containers.Lists.List := Agent.Get_Tasks;
      Prev :          Sancta.Tasks.Task_Id    := Sancta.Tasks.No_Task;
      use Sancta.Tasks.Containers.Lists;

      Total,
      Partial : Sancta.Costs               := 0.0;
      I       : Sancta.Tasks.Containers.Lists.Cursor := T.First;
   begin
      while Has_Element (I) loop
         Partial := Get_Cost (Object'Class (This),
                              Sancta.Agent.Get_Name (Agent),
                              Prev, Sancta.Tasks.Get_Id (Element (I)));
         if Partial = Sancta.Infinite then
            Total := Infinite;
         else
            Total := Total + Partial;
         end if;
         exit when Partial = Sancta.Infinite;
         Prev := Sancta.Tasks.Get_Id (Element (I));
         Next (I);
      end loop;
      return Total;
   end Get_Plan_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object;
      Agent : in String;
      Tasks : in Sancta.Tasks.Containers.Lists.List) return Costs
   is
      Ag : Sancta.Agent.Dummy.Object;
   begin
      Ag.Set_Name (Agent);
      Ag.Set_Tasks (Tasks);
      return Get_Plan_Cost (This, Ag);
   end Get_Plan_Cost;

   -----------
   -- Merge --
   -----------

   procedure Merge (Dst : in out Object; Src : in Object) is
      procedure Do_It (I : in Cursor) is
      begin
         Dst.Matrix.Include (Key (I), Element (I));
      end Do_It;
   begin
      Src.Matrix.Iterate (Do_It'Access);
   end Merge;

   -----------
   -- Print --
   -----------

   procedure Print (This : in Object) is
      procedure Do_It (I : in Cursor) is
      begin
         Log (Key (I) & ": " & To_String (Float (Element (I))), Always);
      end Do_It;
   begin
      Log ("Cost matrix dump follows:", Always);
      This.Matrix.Iterate (Do_It'Access);
   end Print;

   -----------------
   -- Print_Diffs --
   -----------------

   procedure Print_Diffs (L, R : in Object) is
      I : Cursor := L.Matrix.First;
   begin
      Log ("DIFFS IN COST MATRIX", Always);
      while Has_Element (I) loop
         if not R.Matrix.Contains (Key (I)) then
            Log ("Missing key: " & Key (I), Always);
         elsif Element (I) /= Element (R.Matrix.Find (Key (I))) then
            Log (Key (I) & ":" & Element (I)'Img & " /= " &
                 Key (I) & ":" & Element (R.Matrix.Find (Key (I)))'Img,
                 Always);
         end if;

         Next (I);
      end loop;
      Log ("END DIFFS", Always);
   end Print_Diffs;

   --------------
   -- Set_Cost --
   --------------

   procedure Set_Cost
     (This  : in out Object;
      Agent : in     String;
      Ini   : in     Sancta.Tasks.Task_Id;
      Fin   : in     Sancta.Tasks.Task_Id;
      Cost  : in     Costs)
   is
   begin
      Include (This.Matrix, Key (Agent, Ini, Fin), Cost);
   end Set_Cost;

end Sancta.Cost_Matrix;
