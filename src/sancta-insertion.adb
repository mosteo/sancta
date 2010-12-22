with Sancta.Tasks.Extra;
with Sancta.Tasks.Utils;
--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Insertion is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Bids) return Boolean is
   begin
      return L.Cost < R.Cost;
   end "<";

   -------------
   -- Can_Win --
   -------------

   function Can_Win (Bid : Bids) return Boolean is
   begin
      return Bid.Kind /= Invalid;
   end Can_Win;

   -------------
   -- Task_Id --
   -------------

   function Task_Id (Bid : Bids) return Sancta.Tasks.Task_Id is
   begin
      if Bid.Job.Is_Valid then
         return Bid.Job.Ref.Get_Id;
      else
         return Sancta.Tasks.No_Task;
      end if;
   end Task_Id;

   ---------------
   -- Get_Agent --
   ---------------

   function Get_Agent (Bid : Bids) return String is
   begin
      return +Bid.Bot;
   end Get_Agent;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (Bid : Bids) return Costs is
   begin
      return Bid.Cost;
   end Get_Cost;

   --------------
   -- Get_Task --
   --------------

   function Get_Task (Bid : Bids) return Sancta.Tasks.Object'Class is
   begin
      if Bid.Job.Is_Valid then
         return Bid.Job.Get;
      else
         raise Constraint_Error with "No task for this bid";
      end if;
   end Get_Task;

   -----------
   -- Apply --
   -----------

   function Apply (Agent : Sancta.Agent.Object'Class;
                   Bid   : Bids) return Sancta.Agent.Object'Class
   is
   begin
      if Agent.Get_Name /= +Bid.Bot then
         raise Constraint_Error with "Bid - Agent mismatch";
      end if;

      return Apply (Assignment.Create (Agent), Bid).Get_Agent (Agent.Get_Name);
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Ass : Assignment.Object;
      Bid : Bids)
         return Assignment.Object
   is
      Tasks   : Tc.Lists.List;
      New_Ass : Assignment.Object;
   begin
      case Bid.Kind is
         when Insertion =>
            Tasks := Ass.Get_Tasks (+Bid.Bot);
            Sancta.Tasks.Extra.Insert (Tasks, Bid.Job.Ref.all, Bid.Before);
            New_Ass := Ass;
            New_Ass.Set_Tasks (+Bid.Bot, Tasks);
            return New_Ass;
         when Full_List =>
            New_Ass := Ass;
            New_Ass.Set_Tasks (+Bid.Bot, Bid.List);
            return New_Ass;
         when Invalid =>
            raise Constraint_Error with "Bid cannot win";
      end case;
   end Apply;

   -------------
   -- Auction --
   -------------

   function Auction
     (Ass   : Assignment.Object;
      Tasks : Tc.Lists.List;
      Crit  : Assignment_Criteria;
      Cost  : Cost_Cache.Object'Class)
      return Bids
   is
      Best   :          Bids;
      Agents : constant Ac.Lists.List := Ass.Get_Agents;

      procedure Check (I : Ac.Lists.Cursor) is
         Bid : constant Bids :=
                 Auction (Ac.Lists.Element (I), Tasks, Crit, Cost);
      begin
         if Bid < Best then
            Best := Bid;
         end if;
      end Check;
   begin
      Agents.Iterate (Check'Access);

      return Best;
   end Auction;

   -------------
   -- Auction --
   -------------

   function Auction
     (Agent : Sancta.Agent.Object'Class;
      Tasks : Tc.Lists.List;
      Crit  : Assignment_Criteria;
      Cost  : Cost_Cache.Object'Class)
      return Bids
   is
      Best : Bids;

      procedure Check (I : Tc.Lists.Cursor) is
         Bid : constant Bids :=
                 Auction (Agent, Tc.Lists.Element (I), Crit, Cost);
      begin
         if Bid < Best then
            Best := Bid;
         end if;
      end Check;
   begin
      Tasks.Iterate (Check'Access);

      return Best;
   end Auction;

   -------------
   -- Auction --
   -------------

   function Auction
     (Agent      : Sancta.Agent.Object'Class;
      Job        : Sancta.Tasks.Object'Class;
      Crit       : Assignment_Criteria;
      Cost       : Cost_Cache.Object'Class;
      Use_Cached : Boolean := True)
      return Bids
   is
      Best  : Bids;

      Tasks : Tc.Lists.List   := Agent.Get_Tasks;
      I     : Tc.Lists.Cursor := Tasks.First;
   begin
      --  Skip the Not_Before tasks
      for J in 1 .. Agent.Get_Not_Before loop
         if Tc.Lists.Has_Element (I) then
            Tc.Lists.Next (I);
         else
            exit;
         end if;
      end loop;

      --  And try in the remaining positions
      loop
         declare
            New_Agent : Sancta.Agent.Object'Class := Agent.Get_Without_Tasks;
            Pos       : Tc.Lists.Cursor;
            New_Bid   : Bids;
            Before    : Sancta.Tasks.Task_Id := Sancta.Tasks.No_Task;
         begin
            Tasks.Insert (I, Job, Pos); -- insert in current testing position
            New_Agent.Set_Tasks (Tasks);
            Tasks.Delete (Pos);         -- restore original list
            if Tc.Lists.Has_Element (I) then
               Before := Tc.Lists.Element (I).Get_Id;
            end if;

            New_Bid.Cost :=
              Evaluate_Bid (Agent, New_Agent, Crit, Cost, Use_Cached);

            if New_Bid.Cost < Best.Cost then
               Best := (Cost   => New_Bid.Cost,
                        Bot    => +Agent.Get_Name,
                        Job    => Sancta.Tasks.Handle.Set (Job),
                        Kind   => Insertion,
                        Before => Before,
                        List   => Tc.Lists.Empty_List);
            end if;
         end;

         exit when not Tc.Lists.Has_Element (I);
         Tc.Lists.Next (I);
      end loop;

      if Best.Can_Win and then Two_Opt_Enabled then
         declare
            New_Agent : Sancta.Agent.Object'Class := Apply (Agent, Best);
         begin
            while Two_Opt_Enabled loop
               declare
                  Two_Opt_Bid : constant Bids :=
                    Two_Opt (Agent, New_Agent, Job, Crit, Cost, Use_Cached);
               begin
                  if Two_Opt_Bid < Best then
--                       Log ("Best:" & Best.Cost'Img, Always);
--                       Log ("2opt:" & Two_Opt_Bid.Cost'Img, Always);
--                       Log ("2-opt improvement:" & Image (Best.Cost - Two_Opt_Bid.Cost), Always);
                     Best := Two_Opt_Bid;
                     New_Agent.Set_Tasks (Two_Opt_Bid.List);
                  else
                     exit;
                  end if;
               end;
            end loop;
         end;
      end if;

      return Best;
   end Auction;

   ------------------
   -- Evaluate_Bid --
   ------------------

   function Evaluate_Bid (Old_Agent,
                          New_Agent  : Sancta.Agent.Object'Class;
                          Crit       : Assignment_Criteria;
                          Cost       : Sancta.Cost_Cache.Object'Class;
                          Use_Cached : Boolean := True)
                          return      Costs
   is
      function C (A : Sancta.Agent.Object'Class) return Assignment.Object
                     renames Assignment.Create;

      Mm, Ms, Ma : Sancta.Costs := 0.0;
      Mm_New     : Sancta.Costs;

      Old_Ass    : constant Sancta.Assignment.Object := C (Old_Agent);
      New_Ass    : constant Sancta.Assignment.Object := C (New_Agent);
   begin
      if Crit.Minavg_Weight /= 0.0 then
         if Use_Cached then
            Ma :=
              New_Ass.Get_Average_Cost (Cost, False) -
              Old_Ass.Get_Average_Cost (Cost, False);
         else
            Ma :=
              New_Ass.Get_Average_Cost (False) -
              Old_Ass.Get_Average_Cost (False);
         end if;
      end if;

      if Crit.Minmax_Weight /= 0.0 or else Crit.Minsum_Weight /= 0.0 then
         if Use_Cached then
            Mm_New := New_Ass.Get_Max_Min_Cost (Cost);
         else
            Mm_New := New_Ass.Get_Max_Min_Cost;
         end if;
      end if;

      if Crit.Minmax_Weight /= 0.0 then
         Mm := Mm_New;
      end if;

      if Crit.Minsum_Weight /= 0.0 then
         if Use_Cached then
            Ms := Mm_New - Old_Ass.Get_Max_Min_Cost (Cost);
         else
            Ms := Mm_New - Old_Ass.Get_Max_Min_Cost;
            --  This is correct because there's just one agent!
         end if;
      end if;

      return Criteria.Evaluate (Crit, Mm, Ms, Ma);
   end Evaluate_Bid;

   -------------
   -- Two_Opt --
   -------------

   function Two_Opt
     (Old        : Sancta.Agent.Object'Class;
      Agent      : Sancta.Agent.Object'Class;
      Job        : Sancta.Tasks.Object'Class;
      Crit       : Assignment_Criteria;
      Cost       : Cost_Cache.Object'Class;
      Use_Cached : Boolean := True)  return Bids
   is
      Best : Bids;
      Orig : constant Tc.Vectors.Vector := Sancta.Tasks.Utils.To_Vector
        (Agent.Get_Tasks);
   begin
      --  return Best;
      for I in Orig.First_Index .. Orig.Last_Index - 1 loop
         for J in I + 1 .. Orig.Last_Index loop
            declare
               Perm : constant Tc.Vectors.Vector :=
                 Sancta.Tasks.Utils.Reverse_Slice (Orig, I, J);
               New_Bid   : Bids;
               New_Agent : Sancta.Agent.Object'Class := Agent;
            begin
--                 Log (">>>", Always);
--                 Assignment.Create (Agent).Print_Assignment;
               New_Agent.Set_Tasks (Sancta.Tasks.Utils.To_List (Perm));
               New_Bid.Cost :=
                 Evaluate_Bid (Old, New_Agent, Crit, Cost, Use_Cached);
--                 Assignment.Create (New_Agent).Print_Assignment;
--                 Log ("<<<", Always);

               if New_Bid.Cost < Best.Cost then
                  Best := (Cost   => New_Bid.Cost,
                           Bot    => +Agent.Get_Name,
                           Job    => Sancta.Tasks.Handle.Set (Job),
                           Kind   => Full_List,
                           Before => Sancta.Tasks.No_Task,
                           List   => New_Agent.Get_Tasks);
               end if;
            end;
         end loop;
      end loop;

      return Best;
   end Two_Opt;

end Sancta.Insertion;
