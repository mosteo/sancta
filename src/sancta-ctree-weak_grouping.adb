with Sancta.Ctree.Robot;

with Sancta.Agent.Utils;
with Sancta.Types;
with Sancta.Types.Operations;
use  Sancta;

with Agpl.Trace; use Agpl.Trace;
with Agpl.Ustrings; use Agpl.Ustrings;

package body Sancta.Ctree.Weak_Grouping is

   use type Types.Real;

   ------------
   -- Create --
   ------------

   function Create (A : Sancta.Assignment.Object'class;
                    L : Connectivity_Matrix.Object'Class) return Object
   is
   begin
      return Create (A.Get_Agents, L);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (A : Sancta.Agent.Containers.Lists.List;
                    L : Connectivity_Matrix.Object'Class)
      return Object
   is
      This      : Object;
      Av_Agents : Ac.Lists.List := A;

      procedure Add (I : Ac.Lists.Cursor) is
      begin
         This.Agents.Insert (Ac.Lists.Element (I).Get_Name,
                             Ac.Lists.Element (I));
      end Add;
   begin
      This.Links := Connectivity_Matrix.Object (L);
      Av_Agents.Iterate (Add'Access);

      while not Av_Agents.Is_Empty loop
         declare
            Ass :          Sancta.Assignment.Object;
            Ag  : constant Sancta.Agent.Object'Class := Av_Agents.First_Element;
            I   :          Ac.Lists.Cursor;
            Pre :          Ac.Lists.Cursor;
         begin
            Ass.Set_Agent (Av_Agents.First_Element);
            Av_Agents.Delete_First;
            I := Av_Agents.Last;
            while Ac.Lists.Has_Element (I) loop
               Pre := Ac.Lists.Previous (I);
               if L.Are_Weakly_Linked (Ag.Get_Name,
                                       Ac.Lists.Element (I).Get_Name)
               then
                  Ass.Set_Agent (Ac.Lists.Element (I));
                  Av_Agents.Delete (I);
               end if;
               I := Pre;
            end loop;
            This.Groups.Append (Ass);
         end;
      end loop;

      Log ("Groups found:" & This.Groups.Length'Img, Debug, log_section);

      return This;
   end Create;

   --------------------------
   -- Get_Cheapest_Leaders --
   --------------------------

   function Get_Cheapest_Leaders
     (This : Object)
      return Sancta.Agent.Containers.Lists.List
   is
      L : Ac.Lists.List;

      procedure Check (I : Avectors.Cursor) is
      begin
         declare
            A : constant Sancta.Agent.Object'Class :=
                  Avectors.Element (I).Get_Less_Costly_Non_Idle_Agent;
         begin
            L.Append (A);
         end;
      exception
         when Constraint_Error =>
            --  No leader in this group? No tasks or all idle!
            Log ("Group without leader found", Warning, Log_Section);
      end Check;
   begin
      This.Groups.Iterate (Check'Access);
      Log ("Leaders found:" & L.Length'Img, Debug, Log_Section);
      return L;
   end Get_Cheapest_Leaders;

   ---------------
   -- Get_Mates --
   ---------------

   function Get_Mates (This : Object;
                       Bot  : String) return Sancta.Agent.Containers.Lists.List
   is
      use Avectors;
      I : Cursor := First (This.Groups);
   begin
      while Has_Element (I) loop
         if Element (I).Contains (Bot) then
            return Element (I).Get_Agents;
         end if;
         Next (I);
      end loop;

      return Ac.Lists.Empty_List;
   end Get_Mates;

   ---------------
   -- Get_Mates --
   ---------------

   function Get_Mates (This : Object;
                       Bot  : String) return Sancta.Assignment.Object
   is
      use Avectors;
      I : Cursor := First (This.Groups);
   begin
      while Has_Element (I) loop
         if Element (I).Contains (Bot) then
            return Element (I);
         end if;
         Next (I);
      end loop;

      raise Constraint_Error with "Bot not in any group";
   end Get_Mates;

   --------------------
   -- Get_Group_Size --
   --------------------

   function Get_Group_Size (This : Object;
                              Bot  : String) return Natural
   is
   begin
--        Log ("Gorup size for :" & Bot & ":" & This.Get_Mates (Bot).Length'Img,
--             Always);
      return Natural (This.Get_Mates (Bot).Length);
   end Get_Group_Size;

   ----------------
   -- Num_Groups --
   ----------------

   function Num_Groups (This : Object) return Natural is
   begin
      return Natural (This.Groups.Length);
   end Num_Groups;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group (This : Object;
                       Pos  : Positive) return Sancta.Assignment.Object
   is
   begin
      return This.Groups.Element (Pos);
   end Get_Group;

   ------------------------
   -- Merge_With_Closest --
   ------------------------

   procedure Merge_With_Closest (This      :        Object;
                                 Bot       :        String;
                                 From      :        Sancta.Assignment.Object;
                                 New_Links : in out Connectivity_Matrix.Object'Class)
   is
      Own_Pose : constant Sancta.Types.Pose :=
                   Robot.Object (This.Agents.Element (Bot)).Get_Pose;
      Closest_Name : Ustring;
      Closest_Dist : Types.Real := Types.Real'Last;

      use Ac.Maps;
      I : Cursor := This.Agents.First;
   begin
      pragma Assert (This.Num_Groups > 1, "Cannot merge the only group");

      while Has_Element (I) loop
         declare
            Other : constant Robot.Object := Robot.Object (Element (I));
            Dist  : constant Types.Real   :=
                      Types.Operations.Distance (Own_Pose, Other.Get_Pose);
         begin
            if Element (I).Get_Name /= Bot and then
              From.Contains (Element (I).Get_Name) and then
              Dist < Closest_Dist and then
              not New_Links.Are_Weakly_Linked (Bot, Other.Get_Name)
            then
               Closest_Name := + Other.Get_Name;
               Closest_Dist := Dist;
            end if;
         end;
         Next (I);
      end loop;

      pragma Assert (Closest_Dist < Types.Real'Last, "No closest?");
--        Log ("Merging " & Bot & " and " & (+Closest_Name), Always);
--        Log ("Of sizes" & This.Get_Mates (Bot).Length'Img &
--             " and" & This.Get_Mates (+Closest_Name).Length'Img, Always);
      New_Links.Set_Link (Bot, +Closest_Name, 69.0);
   end Merge_With_Closest;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
   begin
      Log ("WEAK GROUPING DUMP", Always);
      for I in This.Groups.First_Index .. This.Groups.Last_Index loop
         Log ("WEAK #" & I'Img, Always);
         This.Groups.Element (I).Print_Assignment;
      end loop;
   end Print;

   ---------------------
   -- Get_Idle_Groups --
   ---------------------

   function Get_Idle_Groups (This : Object) return Object is
      Proto  : Assignment.Object;
   begin
      for I in This.Groups.First_Index .. This.Groups.Last_Index loop
         if This.Groups.Element (I).Get_All_Tasks.Is_Empty then
            proto.Set_Agents (This.Groups.Element (I).Get_Agents);
         end if;
      end loop;

      return Create (Proto, This.Links);
   end Get_Idle_Groups;

   ----------------
   -- Get_Agents --
   ----------------

   function Get_Agents (This : Object) return Ac.Lists.List is
   begin
      return Agent.Utils.To_List (This.Agents);
   end Get_Agents;

end Sancta.Ctree.Weak_Grouping;
