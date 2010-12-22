with Agpl.Trace,
     Sancta.Component.Types,
     Sancta.Tasks.Handle;


package body Sancta.Component.Executor_Root is

   use type Agent.Object_Access;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Thix : in out Object_Preparer)
   is
      This : Object renames Thix.Parent.all;
   begin
      This.Verify (Requires_Agent);
      This.Bot := Agent.Object_Access (Types.Agent (This.Input (Requires_Agent)).Agent);
      pragma Assert (This.Bot /= null);

      if This.Exists (Option_Period) then
         This.Period :=
           Duration'Value (This.Option (Option_Period, This.Period'Img));
      end if;
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      use type Sancta.Tasks.Object'Class,
          Tc.Lists.List;
      Thix : Object'Class renames Object'Class (This);
      Jobs : Tc.Lists.List := This.Bot.Get_Tasks;
   begin
      Next := Clock + This.Period;

      --  Check changes in task list:
      if Jobs /= This.Prev then
         This.Prev := Jobs;
         This.Output (Provides_Task_List, Types.Task_List'(Tasks => Jobs));
         Log ("Notifying task list change", Informative, Log_Section);
      end if;

      if Jobs.Is_Empty then
         Thix.Idle;
      else
         declare
            Job  :          Tasks.Object'Class := Jobs.First_Element;
            Bis  : constant Tasks.Object'Class := Job;
            Done :          Boolean            := False;
         begin
            Thix.Execute (Job, Done);

            if Done then
               Log ("Task completed: " & Job.Image, Informative, Log_Section);
               Jobs.Delete_First;
               This.Bot.Set_Tasks (Jobs);
               This.Output (Provides_Task_Done,
                            Types.Job'(Job => Tasks.Handle.Set (Job)));
            else
               Jobs.Delete_First;
               Jobs.Prepend (Job);
               This.Bot.Set_Tasks (Jobs);
            end if;

            --  Check changes in task progress:
            if Done or else Job /= Bis then
               Log ("Notifying task progress.", Informative, Log_Section);
               This.Output (Provides_Task_List,
                            Types.Task_List'(Tasks => Jobs));
            end if;
         exception
            when E : others =>
               Log ("S.C.Executor_Root.Execute: " & Report (E),
                    Warning, Log_Section);
         end;
      end if;
   end Run;

   -----------
   -- Agent --
   -----------

   function Get_Agent (This : Object) return access Sancta.Agent.Object'Class is
   begin
      pragma Assert (This.Bot /= null);
      return This.Bot;
   end Get_Agent;

end Sancta.Component.Executor_Root;
