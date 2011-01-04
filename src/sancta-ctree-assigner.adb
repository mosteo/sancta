with Sancta.Agent;
with Agpl.Trace; use Agpl.Trace;
with Agpl;

with Ada.Containers.Indefinite_Ordered_Maps;

package body Sancta.Ctree.Assigner is

   package Agent_Lists renames Sancta.Agent.Containers.Lists;

   package Strat_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Alloc_Strategies, Object'Class);

   use type Sancta.Tasks.Object'Class;

   Strategies : Strat_Maps.Map;

   -------------------
   -- Remove_Linked --
   -------------------

   procedure Remove_Linked
     (Ass   : in out Sancta.Assignment.Object'Class;
      Bot   :        String;
      Links :        Connectivity_Matrix.Object'Class)
   is
      use Agent_Lists;
      Agents : constant List   := Ass.Get_Agents;
      I      :          Cursor := Agents.First;
   begin
      while Has_Element (I) loop
         if Links.Are_Weakly_Linked (Bot, Element (I).Get_Name) then
            pragma Assert (Element (I).Get_Tasks.Is_Empty, "Agent has tasks!");
            Ass.Remove_Agent (Element (I).Get_Name);
         end if;
         Next (I);
      end loop;
   end Remove_Linked;

   ----------------------
   -- Factory_Register --
   ----------------------

   procedure Factory_Register (Strat : Alloc_Strategies;
                               This  : Object'Class)
   is
   begin
      Strategies.Insert (Strat, This);
   end Factory_Register;

   --------------------
   -- Factory_Create --
   --------------------

   function Factory_Create   (Strat : Alloc_Strategies) return Object'Class is
   begin
      return Strategies.Element (Strat);
   exception
      when others =>
         Log ("Cannot create assigner: " & Strat'Img, Error);
         Log ("Not registered.", Error);
         raise;
   end Factory_Create;

   --------------------
   -- Copy_To_Linked --
   --------------------

   procedure Copy_To_Linked (Ass   : in out Sancta.Assignment.Object'Class;
                             Bot   : in     String;
                             Links :        Connectivity_Matrix.Object'Class)
   is
      T : constant Tc.Lists.List := Ass.Get_Agent (Bot).Get_Tasks;
      A : constant Ac.Lists.List := Ass.Get_Agents;
      procedure Copy (I : Ac.Lists.Cursor) is
         Bot2 : Sancta.Agent.Object'Class :=
           Sancta.Agent.Object'Class (Ac.Lists.Element (I));
      begin
         if Bot /= Bot2.Get_Name then
            if Links.Are_Weakly_Linked (Bot, Bot2.Get_Name) then
               if Bot2.Has_Tasks then
                  if not T.Is_Empty and then
                    T.First_Element /= Bot2.Get_First_Task
                  then
                     Log ("WARNING: Propagating to robot with tasks",
                          Warning, Log_Section);
                     --  raise Constraint_Error;
                  end if;
               end if;
               Bot2.Set_Tasks (T);
               Ass.Set_Agent (Bot2);
            end if;
         end if;
      end Copy;
   begin
      A.Iterate (Copy'Access);
   end Copy_To_Linked;

   --------------------
   -- Copy_To_Linked --
   --------------------

   procedure Copy_To_Linked (Ass   : in out Sancta.Assignment.Object'Class;
                             Bots  :        Sancta.Agent.Containers.Lists.List;
                             Links :        Connectivity_Matrix.Object'Class)
   is
      procedure Do_It (I : Ac.Lists.Cursor) is
      begin
         Copy_To_Linked (Ass, Ac.Lists.Element (I).Get_Name, Links);
      end Do_It;
   begin
      Bots.Iterate (Do_It'Access);
   end Copy_To_Linked;

end Sancta.Ctree.Assigner;
