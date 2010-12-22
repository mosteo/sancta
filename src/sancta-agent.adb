with Agpl.Conversions; use Agpl.Conversions;
with Sancta.Tasks.Starting_Pose;

package body Sancta.Agent is

   use type Sancta.Costs;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Object) return Boolean is
   begin
      return L.Get_Name = R.Get_Name;
   end "=";

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (This     : in out Object;
      The_Task : in Sancta.Tasks.Object'Class)
   is
   begin
      This.Tasks.Append (The_Task);
   end Add_Task;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks (This : in out Object; T : Tc.Lists.List) is
      use Tc.Lists;
      procedure Add (I : Cursor) is
      begin
         This.Add_Task (Element (I));
      end Add;
   begin
      T.Iterate (Add'Access);
   end Add_Tasks;

   -----------------------
   -- Add_Task_Executer --
   -----------------------

   procedure Add_Task_Executer (This : in out Object;
                                Job  : in     Ada.Tags.Tag;
                                Exe  : in     Task_Executer)
   is
   begin
      Executer_Maps.Include (This.Execs, External_Tag (Job), Exe);
   end Add_Task_Executer;

   -------------------
   -- Call_Executer --
   -------------------

   procedure Call_Executer (This : in out Object;
                            Job  : in out Sancta.Tasks.Object'Class;
                            Done : in out Boolean)
   is
      use Executer_Maps;
      I : constant Cursor := This.Execs.Find (External_Tag (Job'Tag));
   begin
      if Has_Element (I) then
         Element (I).all (Object'Class (This), Job, Done);
      else
         Log ("Sancta.Agent.Call_Executer: Unknown task: " &
              External_Tag (Job'Tag),
              Warning);
      end if;
   end Call_Executer;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This : in out Object) is
   begin
      This.Tasks.Clear;
   end Clear_Tasks;

   -----------------------
   -- Execute_When_Idle --
   -----------------------

   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Sancta.Plan.Object)
   is
      pragma Unreferenced (This, Plan);
   begin
      null;
   end Execute_When_Idle;

   ---------------------
   -- Get_Cached_Cost --
   ---------------------

   function Get_Cached_Cost (This : in Object) return Costs is
   begin
      return This.Cost;
   end Get_Cached_Cost;

   -----------------
   -- Get_Elapsed --
   -----------------

--     function Get_Elapsed (This : in Object) return Duration is
--        use Ada.Calendar;
--     begin
--        return Clock - This.Start;
--     end Get_Elapsed;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This     : in Object;
      The_Task : in Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
   begin
      if The_Task in Sancta.Tasks.Starting_Pose.Object then
         return 0.0;
      else
         return Get_Cost (Sancta.Agent.Object'Class (This), -- Force dispatching
                          Sancta.Tasks.Starting_Pose.Create
                            (Sancta.Agent.Object'Class (This).Get_Name),
                          The_Task);
      end if;
   end Get_Cost;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : in Object;
      From, To : in Sancta.Tasks.Object'Class)
      return Costs
   is
      pragma Unreferenced (This, From, To);
   begin
      return Costs'Last;
   end Get_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost (This : in Object) return Costs is
   begin
      return Get_Plan_Cost (This, This.Tasks);
   end Get_Plan_Cost;

   -------------------
   -- Get_Plan_Cost --
   -------------------

   function Get_Plan_Cost (This  : in Object;
                           Tasks : in Sancta.Tasks.Containers.Lists.List) return Costs
   is
      use Sancta.Tasks.Containers.Lists;
      I     : Cursor := First (Tasks);
      Prev  : Cursor;
      Total : Costs  := 0.0;
      Cost  : Costs;
   begin
      while I /= No_Element loop
         if not Has_Element (Prev) then
            Cost := Get_Cost (Object'Class (This), Element (I));
            if Cost < Infinite then
               Total := Total + Cost;
            else
               return Infinite;
            end if;
         else
            Cost := Get_Cost (Object'Class (This), Element (Prev), Element (I));
            if Cost < Infinite then
               Total := Total + Cost;
            else
               return Infinite;
            end if;
         end if;
         Prev := I;
         Next (I);
      end loop;

      return Total;
   exception
      when Constraint_Error =>
         return Infinite;
   end Get_Plan_Cost;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : in Object) return String is
   begin
      return S (This.Name);
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name2 (This : in Object'Class) return String is
   begin
      return S (This.Name);
   end Get_Name2;

   -----------------------
   -- Get_Without_Tasks --
   -----------------------

   function Get_Without_Tasks (This : in Object) return Object'Class is
      Clon : Object'Class := This;
   begin
      Clon.Clear_Tasks;
      return Clon;
   end Get_Without_Tasks;

   --------------------
   -- Get_First_Task --
   --------------------

   function Get_First_Task (This : in Object) return Sancta.Tasks.Object'Class
   is
      use Sancta.Tasks.Containers.Lists;
   begin
      return Element (First (This.Tasks));
   end Get_First_Task;

   -------------------
   -- Get_Last_Task --
   -------------------

   function Get_Last_Task  (This : in Object) return Sancta.Tasks.Object'Class
   is
      use Sancta.Tasks.Containers.Lists;
   begin
      return Element (Last (This.Tasks));
   end Get_Last_Task;

   ------------------
   -- Has_Executer --
   ------------------

   function Has_Executer (This : in Object;
                          Job  : in Sancta.Tasks.Object'Class) return Boolean
   is
   begin
      return This.Execs.Contains (External_Tag (Job'Tag));
   end Has_Executer;

   ---------------
   -- Has_Tasks --
   ---------------

   function Has_Tasks (This : in Object) return Boolean is
      use Sancta.Tasks.Containers.Lists;
   begin
      return not Is_Empty (This.Tasks);
   end Has_Tasks;

   --------------------
   -- Get_Task_Count --
   --------------------

   function Get_Task_Count (This : in Object) return Natural is
   begin
      return Natural (This.Tasks.Length);
   end Get_Task_Count;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : in Object)
                       return Sancta.Tasks.Containers.Lists.List is
   begin
      return This.Tasks;
   end Get_Tasks;

   ----------------
   -- Mark_Start --
   ----------------

--     procedure Mark_Start (This : in out Object) is
--     begin
--        This.Start := Ada.Calendar.Clock;
--        This.Cost  := Get_Plan_Cost (This);
--     end Mark_Start;

   ---------------------
   -- Check_Task_List --
   ---------------------

   procedure Check_Task_List (This     : Object;
                              Checker  : access procedure
                                (Tasks : Sancta.Tasks.Containers.Lists.List)) is
   begin
      Checker (This.Tasks);
   end Check_Task_List;

   ----------------------
   -- Modify_Task_List --
   ----------------------

   procedure Modify_Task_List (This     : in out Object;
                               Modifier : access procedure
                                 (Tasks : in out Sancta.Tasks.Containers.Lists.List))
   is
   begin
      Modifier (This.Tasks);
   end Modify_Task_List;

   --------------
   -- Set_Task --
   --------------

   procedure Set_Task (This : in out Object;
                       T    :        Sancta.Tasks.Object'Class)
   is
   begin
      This.Clear_Tasks;
      This.Add_Task (T);
   end Set_Task;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Sancta.Tasks.Containers.Lists.List) is
   begin
      This.Tasks := Tasks;
   end Set_Tasks;

   -----------------------
   -- Remove_First_Task --
   -----------------------

   procedure Remove_First_Task (This : in out Object) is
   begin
      This.Remove_Task (This.Get_First_Task.Get_Id);
   end Remove_First_Task;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task (This : in out Object; Id : in Sancta.Tasks.Task_Id) is
      use Sancta.Tasks.Containers.Lists;
      use type Sancta.Tasks.Task_Id;
      I : Cursor := First (This.Tasks);
   begin
      while I /= No_Element loop
         if Sancta.Tasks.Get_Id (Element (I)) = Id then
            Delete (This.Tasks, I);
            return;
         end if;
         Next (I);
      end loop;
   end Remove_Task;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This : in out Object; Name : in String) is
   begin
      This.Name := U (Name);
   end Set_Name;

   ---------------------
   -- Print_Plan_Cost --
   ---------------------

   procedure Print_Plan_Cost (This : in Object)
   is
      use Sancta.Tasks.Containers.Lists;
      Tasks : constant Sancta.Tasks.Containers.Lists.List := This.Get_Tasks;
      Total : Sancta.Costs := 0.0;

      Res   : Ustring;

      use ASU;
   begin
      Append (Res, This.Name);
      Append (Res, ": ");

      if Tasks.Is_Empty then
         Append (Res, "(0)");
      else
         Append (Res, "(");
         Append (Res, To_String (Float (Get_Cost (Object'Class (This),
                                                  First_Element (Tasks)))));
         Append (Res, ")");
         Append (Res, Tasks.First_Element.Get_Id'Img);
         Total := Total + Get_Cost (Object'Class (This), First_Element (Tasks));

         declare
            Prev : Cursor := Tasks.First;
            Curr : Cursor := Next (Tasks.First);
         begin
            while Has_Element (Curr) loop
               Append (Res, " (");
               Append (Res, To_String (Float (Get_Cost (Object'Class (This),
                                                        Element (Prev),
                                                        Element (Curr)))));
               Append (Res, ")");
               Append (Res, Element (Curr).Get_Id'Img);
               Total := Total + Get_Cost (Object'Class (This),
                                          Element (Prev),
                                          Element (Curr));

               Prev := Curr;
               Next (Curr);
            end loop;
            Append (Res, " (Total:" & To_String (Float (Total)) & ")");
         end;
      end if;

      Log (+Res, Debug);
   end Print_Plan_Cost;

   --------------------
   -- Set_Not_Before --
   --------------------

   procedure Set_Not_Before (This : in out Object;
                             Pos  :        Natural)
   is
   begin
      This.Not_Before := Pos;
   end Set_Not_Before;

   --------------------
   -- Get_Not_Before --
   --------------------

   function Get_Not_Before (This : Object) return Natural is
   begin
      return This.Not_Before;
   end Get_Not_Before;

   --------------
   -- Finished --
   --------------

   function Finished (This : Object;
                      Job  : Tasks.Object'Class) return Boolean
   is
      pragma Unreferenced (This, Job);
   begin
      raise Program_Error with "Must be overriden";
      return False;
   end Finished;

end Sancta.Agent;
