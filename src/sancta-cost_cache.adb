with Sancta.Agent.Dummy,
     Sancta.Agent.Utils;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Cost_Cache is

   use type Sancta.Costs;
   use type Sancta.Tasks.Task_Id;
--     package Ac renames Sancta.Agent.Containers;
   package Tc renames Sancta.Tasks.Containers;
   package Al renames Sancta.Agent.Containers.Lists;
   package Tl renames Sancta.Tasks.Containers.Lists;

   ---------------
   -- Add_Costs --
   ---------------

   procedure Add_Costs
     (This   : in out Object;
      Agents : in Sancta.Agent.Containers.Lists.List;
      Tasks  : in Sancta.Tasks.Containers.Lists.List)
   is
      use type AL.Cursor; use type TL.Cursor;

      Thix : Object'Class renames Object'Class (This);

      A   : AL.Cursor := AL.First (Agents);
      Ini,
      Fin : TL.Cursor;
   begin
      while A /= AL.No_Element loop
         declare
            Bot   : constant Sancta.Agent.Object'Class := Al.Element (A);
            Name  : constant String                    := Bot.Get_Name;
         begin
            Ini := Tl.First (Tasks);
            while Ini /= Tl.No_Element loop

               Fin := Tl.First (Tasks);
               while Fin /= Tl.No_Element loop

                  if Ini /= Fin then
                     declare
                        Tini  : constant Sancta.Tasks.Object'Class :=
                                  Tl.Element (Ini);
                        Tfin  : constant Sancta.Tasks.Object'Class :=
                                  Tl.Element (Fin);
                        Iniid : constant Sancta.Tasks.Task_Id := Tini.Get_Id;
                        Finid : constant Sancta.Tasks.Task_Id := Tfin.Get_Id;
                     begin
                        Set_Cost (Thix, Name, Iniid, Finid,
                                  Bot.Get_Cost (Tini, Tfin));
                        Set_Cost (Thix, Name, Finid, Iniid,
                                  Bot.Get_Cost (Tfin, Tini));
                     end;
                  else
                     --  Same task... may be should be Infinite, maybe zero?
                     pragma Ummmm;
                     null;
                  end if;

                  Tl.Next (Fin);
               end loop;
               Set_Cost (Thix, Name,
                         Sancta.Tasks.No_Task,
                         Tl.Element (Ini).Get_Id,
                         Bot.Get_Cost (Tl.Element (Ini)));
               Tl.Next (Ini);
            end loop;
         end;
         AL.Next (A);
      end loop;

      --  Add the No_Task specials
      --  This is open to interpretation, what if the end task is No_Task?

--        A := Agents.First;
--        while Al.Has_Element (A) loop
--           Ini := Tasks.First;
--           while Tl.Has_Element (Ini) loop
--
--              Set_Cost (This,
--                        Sancta.Agent.Get_Name (Al.Element (A)),
--                        Sancta.Tasks.Get_Id (Tl.Element (Ini)),
--                        Sancta.Tasks.No_Task,
--                        0.0);
--
--              Tl.Next (Ini);
--           end loop;
--           Al.Next (A);
--        end loop;
   end Add_Costs;

   ---------------
   -- Add_Costs --
   ---------------

   procedure Add_Costs
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class)
   is
      use Sancta.Agent.Utils;
   begin
      This.Add_Costs (+Agent, Agent.Get_Tasks);
   end Add_Costs;

   ---------------
   -- Add_Costs --
   ---------------

   procedure Add_Costs
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class;
      Job   :        Tasks.Object'Class)
   is
      Thix : Object'Class renames Object'Class (This);

      Jobs : constant Tc.Lists.List := Agent.Get_Tasks;

      procedure Add_Costs (I : Tc.Lists.Cursor) is
         Old_Job : constant Tasks.Object'Class := Tc.Lists.Element (I);
      begin
         if not Thix.Contains (Agent.Get_Name,
                               Tasks.No_Task,
                               Old_Job.Get_Id)
         then
            Thix.Set_Cost
              (Agent.Get_Name,
               Tasks.No_Task,
               Old_Job.Get_Id,
               Agent.Get_Cost (Old_Job));
         end if;

         Thix.Set_Cost
              (Agent.Get_Name,
               Job.Get_Id,
               Old_Job.Get_Id,
               Agent.Get_Cost (Job, Old_Job));

         Thix.Set_Cost
              (Agent.Get_Name,
               Old_Job.Get_Id,
               Job.Get_Id,
               Agent.Get_Cost (Old_Job, Job));
      end Add_Costs;
   begin
      Thix.Set_Cost
        (Agent.Get_Name,
         Tasks.No_Task,
         Job.Get_Id,
         Agent.Get_Cost (Job));

      Jobs.Iterate (Add_Costs'Access);
   end Add_Costs;

   ------------------
   -- Add_Initials --
   ------------------

   procedure Add_Initials
     (This  : in out Object;
      Agent :        Sancta.Agent.Object'Class)
   is
      Thix  : Object'Class renames Object'Class (This);
      Tasks : constant Tc.Lists.List := Agent.Get_Tasks;
      T     : Tc.Lists.Cursor        := Tasks.First;
      Name  : constant String        := Agent.Get_Name;
   begin
      while Tc.Lists.Has_Element (T) loop
         Set_Cost (Thix,
                   Name,
                   Sancta.Tasks.No_Task,
                   Sancta.Tasks.Get_Id (Tc.Lists.Element (T)),
                   Agent.Get_Cost      (Tc.Lists.Element (T)));
         Tc.Lists.Next (T);
      end loop;
   end Add_Initials;

   ------------------
   -- Add_Initials --
   ------------------

   procedure Add_Initials
     (This   : in out Object;
      Agents :        Sancta.Agent.Containers.Lists.List)
   is
      use Al;
      procedure Add_Initials_Inner (I : Cursor) is
      begin
         This.Add_Initials (Element (I));
      end Add_Initials_Inner;
   begin
      Agents.Iterate (Add_Initials_Inner'Access);
   end Add_Initials;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost
     (This  : in Object'Class;
      Agent : in Sancta.Agent.Object'Class)
      return Costs
   is
      T    : constant Sancta.Tasks.Containers.Lists.List := Agent.Get_Tasks;
      Prev :          Sancta.Tasks.Task_Id               := Sancta.Tasks.No_Task;
      use Sancta.Tasks.Containers.Lists;

      Total,
      Partial : Sancta.Costs                          := 0.0;
      I       : Sancta.Tasks.Containers.Lists.Cursor := T.First;
   begin
      while Has_Element (I) loop
         Partial := Get_Cost (This,
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
     (This  : in Object'Class;
      Agent : in String;
      Tasks : in Sancta.Tasks.Containers.Lists.List)
      return Costs
   is
      Ag : Sancta.Agent.Dummy.Object;
   begin
      Ag.Set_Name (Agent);
      Ag.Set_Tasks (Tasks);
      return Get_Plan_Cost (This, Ag);
   end Get_Plan_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This  : in Empty_Class;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Costs
   is
      pragma Unreferenced (This, Agent, Ini, Fin);
   begin
      return Sancta.Infinite;
   end Get_Cost;

   --------------
   -- Contains --
   --------------

   function Contains
     (This  : in Empty_Class;
      Agent : in String;
      Ini   : in Sancta.Tasks.Task_Id;
      Fin   : in Sancta.Tasks.Task_Id) return Boolean
   is
      pragma Unreferenced (This, Agent, Ini, Fin);
   begin
      return False;
   end Contains;

end Sancta.Cost_Cache;
