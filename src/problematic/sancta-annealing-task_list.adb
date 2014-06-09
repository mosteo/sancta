with Agpl.Random;
with Sancta.Tasks;

package body Sancta.Annealing.Task_List is

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : Solution) return Tc.Lists.List is
   begin
      return This.Tasks;
   end Get_Tasks;

   ------------
   -- Create --
   ------------

   function Create
     (Bot   : Agent.Object'Class;
      Tasks : Tc.Lists.List)
      return Solution
   is
   begin
      return (Agpl.Optimization.Annealing.Solution.Object with
              Bot   => Agent.Handle.Set (Bot),
              Tasks => Tasks,
              Prev  => Tasks);
   end Create;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (This : Solution)
      return Cost
   is
      C : constant Costs := This.Bot.Ref.Get_Plan_Cost (This.Tasks);
   begin
      if C < Infinite then
         return Agpl.Optimization.Cost (C);
      else
         return Agpl.Optimization.Infinite;
      end if;
   end Evaluate;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (This : in out Agpl.Optimization.Annealing.Solution.Object'Class)
   is
   begin
      declare
         This : Solution renames Solution (Revert.This);
      begin
         This.Tasks := This.Prev;
      end;
   end Revert;

   -------------------
   -- Mutation_Swap --
   -------------------

   procedure Mutation_Swap
     (This : in out Agpl.Optimization.Annealing.Solution.Object'Class) is
   begin
      declare
         This : Solution renames Solution (Mutation_Swap.This);
      begin
         This.Prev := This.Tasks;

         if Natural (This.Tasks.Length) <= 1 then
            return;
         end if;

         declare
            use Agpl;
            First  : constant Positive :=
              Random.Get_Integer (1, Positive (This.Tasks.Length) - 1);
            Second : constant Positive :=
              Random.Get_Integer (First + 1, Positive (This.Tasks.Length));

            use Tc.Lists;
            Ic, J, K   : Cursor;
         begin
            Log ("Swapping" & First'Img & " and" & Second'Img,
                 Debug, Det_Section);

            Ic := This.Tasks.First;

            for I in 1 .. Second loop
               if I = First then
                  J := Ic;
               elsif I = Second then
                  K := Ic;
               end if;
               Next (Ic);
            end loop;

            pragma Assert (Has_Element (J) and then Has_Element (K));

            declare
               Temp : constant Tasks.Object'Class := Element (J);
            begin
               This.Tasks.Replace_Element (J, Element (K));
               This.Tasks.Replace_Element (K, Temp);
            end;
         end;
      end;
   end Mutation_Swap;

end Sancta.Annealing.Task_List;
