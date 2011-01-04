with Agpl.Boost.Graphs;
with Agpl.Conversions;
with Agpl.Text_Io; use Agpl.Text_Io;
with Agpl.Ustrings;
with Agpl.Trace;
use Agpl;

package body Sancta.Ctree.Connectivity_Matrix is

   function K (S1, S2 : String) return Key; pragma Inline (K);
   function K (S1, S2 : String) return Key is
   begin
      if S1 < S2 then
         return (S1'Length, S2'Length, S1, S2);
      else
         return (S2'Length, S1'Length, S2, S1);
      end if;
   end K;

   function "<" (L, R : Key) return Boolean is
   begin
      return L.R1 < R.R1 or else
       (L.R1 = R.R1 and then L.R2 < R.R2);
   end "<";

   ----------------
   -- Set_Umbral --
   ----------------

   procedure Set_Umbral (This   : in out Object;
                         Umbral :        Link_Qualities) is
   begin
      This.Umbral := Umbral;
   end Set_Umbral;

   --------------
   -- Set_Link --
   --------------

   procedure Set_Link (This   : in out Object;
                       R1, R2 :        String;
                       Q      : Link_Qualities) is
   begin
      This.Include (K (R1, R2), Q);
   end Set_Link;

   --------------
   -- Contains --
   --------------

   function Contains (This : Object; R1, R2 : String) return Boolean is
   begin
      return This.Contains (K (R1, R2));
   end Contains;

   ----------------
   -- Get_Umbral --
   ----------------

   function Get_Umbral (This : Object) return Link_Qualities is
   begin
      return This.Umbral;
   end Get_Umbral;

   --------------
   -- Get_Link --
   --------------

   function Get_Link (This : Object; R1, R2 : String) return Link_Qualities is
   begin
      return This.Element (K (R1, R2));
   exception
      when Constraint_Error =>
         Put_Line ("Missing link: " & R1 & "--" & R2);
         return This.Element (K (R1, R2));
   end Get_Link;

   ----------------
   -- Get_Robots --
   ----------------

   function Get_Robots (This : Object)
                        return Agpl.Containers.String_Sets.Set
   is
      Result : Containers.String_Sets.Set;

      procedure Check (I : Link_Maps.Cursor) is
      begin
         Result.Include (Link_Maps.Key (I).R1);
         Result.Include (Link_Maps.Key (I).R2);
      end Check;
   begin
      This.Iterate (Check'Access);

      return Result;
   end Get_Robots;

   ----------------
   -- Get_Leaves --
   ----------------

   function Get_Leaves (This : Object)
                        return Agpl.Containers.String_Sets.Set
   is
      Result : Containers.String_Sets.Set;

      procedure Check (I : Link_Maps.Cursor) is
      begin
         if This.Is_Leaf (Link_Maps.Key (I).R1) then
            Result.Include (Link_Maps.Key (I).R1);
         end if;
         if This.Is_Leaf (Link_Maps.Key (I).R2) then
            Result.Include (Link_Maps.Key (I).R2);
         end if;
      end Check;
   begin
      This.Iterate (Check'Access);

      return Result;
   end Get_Leaves;

   -------------
   -- Is_Weak --
   -------------

   function Is_Weak (This : Object; R1, R2 : String) return Boolean is
   begin
      return not This.Contains (R1, R2) or else This.Is_Weak (K (R1, R2));
   end Is_Weak;

   -------------
   -- Is_Weak --
   -------------

   function Is_Weak (This : Object; Bots : Key) return Boolean is
   begin
      pragma Assert (This.Umbral > 0.0);
      return This.Element (Bots) > Link_Qualities (This.Umbral);
   end Is_Weak;

   -----------------------
   -- Are_Weakly_Linked --
   -----------------------

   function Are_Weakly_Linked (This   : Object;
                               R1, R2 : String;
                               Hops   : Positive := Positive'Last)
                               return Boolean
   is
      Linked : Agpl.Containers.String_Sets.Set;
      Dist   : Positive := 1;
   begin
      if R1 = R2 then return False; end if;

      Linked.Insert (R1);

      loop
         declare
            Added : Boolean := False;

            use Link_Maps;
            I     : Cursor := This.First;
         begin
            while Has_Element (I) loop
               declare
                  Bots : constant Connectivity_Matrix.Key := Link_Maps.Key (I);
               begin
                  if Linked.Contains (Bots.R1) and then
                    This.Is_Weak (Bots.R1, Bots.R2)
                  then
                     if Bots.R2 = R2 then
                        --  Log ("Weaklinked: " & R1 & " -- " & R2, Always);
                        return True;
                     elsif not Linked.Contains (Bots.R2) then
                        Linked.Insert (Bots.R2);
                        Added := True;
                     end if;
                  elsif Linked.Contains (Bots.R2) and then
                    This.Is_Weak (Bots.R1, Bots.R2)
                  then
                     if Bots.R1 = R2 then
                        --  Log ("Weaklinked: " & R1 & " -- " & R2, Always);
                        return True;
                     elsif not Linked.Contains (Bots.R1) then
                        Linked.Insert (Bots.R1);
                        Added := True;
                     end if;
                  end if;
               end;
               Next (I);
            end loop;
            exit when Dist = Hops or else not Added;
         end;
         Dist := Dist + 1;
      end loop;

      pragma Assert (Dist = Hops or else not Linked.Contains (R2));

      return False;
   end Are_Weakly_Linked;

   ----------------
   -- Weak_Count --
   ----------------

   function Weak_Count (This : Object) return Natural is
      use Link_Maps;
      Count : Natural := 0;
      procedure Check (I : Cursor) is
         Bots : constant Key := Link_Maps.Key (I);
      begin
         if This.Is_Weak (Bots.R1, Bots.R2) then
            Count := Count + 1;
         end if;
      end Check;
   begin
      This.Iterate (Check'Access);
      return Count;
   end Weak_Count;

   --------------------
   -- Has_Weak_Links --
   --------------------

   function Has_Weak_Links (This : Object;
                            Bot  : String) return Boolean
   is
      use Link_Maps;

      I : Cursor := This.First;
   begin
      while Has_Element (I) loop
         declare
            Bots : constant Key := Link_Maps.Key (I);
         begin
            if (Bots.R1 = Bot or else Bots.R2 = Bot) and then
              This.Is_Weak (Bots.R1, Bots.R2)
            then
               return True;
            end if;
         end;
         Next (I);
      end loop;

      return False;
   end Has_Weak_Links;

   -------------------
   -- Same_Topology --
   -------------------

   function Same_Topology (L, R : Object) return Boolean is
      use Link_Maps;

      I : Cursor := L.First;
   begin
      while Has_Element (I) loop
         declare
            Bots : constant Key := Link_Maps.Key (I);
         begin
            if L.Is_Weak (Bots.R1, Bots.R2) and then not R.Is_Weak (Bots.R1, Bots.R2) then
               return False;
            end if;
         end;
         Next (I);
      end loop;

      I := R.First;
      while Has_Element (I) loop
         declare
            Bots : constant Key := Link_Maps.Key (I);
         begin
            if R.Is_Weak (Bots.R1, Bots.R2) and then
              not L.Is_Weak (Bots.R1, Bots.R2)
            then
               return False;
            end if;
         end;
         Next (I);
      end loop;

      return True;
   end Same_Topology;

   -------------------
   -- Same_Topology --
   -------------------

   function Same_Topology (L, R : Object;
                           Bot  : String) return Boolean
   is
      Not_Same : exception;

      use Containers.String_Sets;

      Bots : constant Containers.String_Sets.Set :=
        L.Get_Robots or R.Get_Robots;

      procedure Check (I : Containers.String_Sets.Cursor) is
      begin
         if Bot /= Element (I) then
            if L.Are_Weakly_Linked (Bot, Element (I)) /=
               R.Are_Weakly_Linked (Bot, Element (I))
            then
               raise Not_Same;
            end if;
         end if;
      end Check;
   begin
      Bots.Iterate (Check'Access);
      return True;
   exception
      when Not_Same =>
         return False;
   end Same_Topology;

   -----------
   -- Print --
   -----------

   procedure Print (This : Object) is
      use Link_Maps;
      Weakness : constant array (Boolean) of String (1 .. 6) :=
                   (True => "Weak  ", False => "Strong");
      procedure Print_Link (I : Cursor) is
         Bots : constant Key := Link_Maps.Key (I);
         use Agpl.Conversions;
      begin
--           Put_Line (Bots.R1 & " -- " & Bots.R2 & ": " &
--                     To_String (Float (Element (I))) & " " &
--                     Weakness (This.Is_Weak (Bots.R1, Bots.R2)));
         Log (Bots.R1 & " -- " & Bots.R2 & ": " &
              To_String (Float (Element (I))) & " " &
              Weakness (This.Is_Weak (Bots.R1, Bots.R2)),
              Always, Log_Section);
      end Print_Link;
   begin
      This.Iterate (Print_Link'Access);
   end Print;

   ----------
   -- Prim --
   ----------

   function Prim (This : Object) return Object is
      use Agpl.Boost.Graphs; use type Weight;
      use Agpl.Containers;
      use Agpl.Ustrings;

      Bots : constant String_Sets.Set := This.Get_Robots;
      W    : Weight_Matrix (1 .. Natural (Bots.Length),
                            1 .. Natural (Bots.Length)) :=
                                      (others => (others => Inf_Weight));
      Bot_Names : Ustring_Array (1 .. Natural (Bots.Length));
      I : String_Sets.Cursor := Bots.First;
   begin
      for J in Bot_Names'Range loop
         Bot_Names (J) := + String_Sets.Element (I);
         String_Sets.Next (I);
      end loop;

      for I in Bot_Names'Range loop
         for J in Bot_Names'Range loop
            if This.Contains (+Bot_Names (I), +Bot_Names (J)) then
               W (I, J) :=
                 Weight (This.Get_Link (+Bot_Names (I), +Bot_Names (J)));
            end if;
         end loop;
      end loop;

      declare
         Tree   : constant Weight_Matrix := Prim (W);
         Result :          Object        := This;
      begin
         Result.Clear;

         for I in Bot_Names'Range loop
            for J in Bot_Names'Range loop
               if Tree (I, J) >= 0.0 and then Tree (I, J) < Inf_Weight then
                  Result.Set_Link (+Bot_Names (I), +Bot_Names (J),
                                   Link_Qualities (Tree (I, J)));
               end if;
            end loop;
         end loop;

         return Result;
      end;
   end Prim;

   ---------------
   -- Neighbors --
   ---------------

   function Neighbors (This : Object;
                       Bot  : String;
                       Only_Weak : Boolean := False)
                       return Agpl.Containers.String_Sets.Set
   is
      Result : Containers.String_Sets.Set;

      procedure Check (I : Link_Maps.Cursor) is
         Pair : constant Key := Link_Maps.Key (I);
      begin
         if Pair.R1 = Bot then
            if not Only_Weak or else Link_Maps.Element (I) >= This.Umbral then
               Result.Include (Pair.R2);
            end if;
         elsif Pair.R2 = Bot then
            if not Only_Weak or else Link_Maps.Element (I) >= This.Umbral then
               Result.Include (Pair.R1);
            end if;
         end if;
      end Check;
   begin
      This.Iterate (Check'Access);
      return Result;
   end Neighbors;

   ------------------------
   -- Depth_First_Search --
   ------------------------

   function Depth_First_Search (This : Object;
                                From,
                                To   : String)
                                return Agpl.Containers.String_Vectors.Vector
   is
      Result  : Containers.String_Vectors.Vector;
      Bailout : Boolean := False;

      use Containers;

      ----------
      -- Dive --
      ----------

      procedure Dive (Solution : String_Vectors.Vector;
                      New_Node : String)
      is
      begin
         --  Break recursion
         if Bailout then
            return;
         end if;

         --  Or check completion
         declare
            New_Solution : String_Vectors.Vector := Solution;
         begin
            New_Solution.Append (New_Node);
            if New_Node = To then
               Result  := New_Solution;
               Bailout := True;
               return;
            end if;

            --  Or check neighbors
            declare
               Near : constant String_Sets.Set :=
                        This.Neighbors (New_Node,
                                        Only_Weak => False);
               use String_Sets;
               I    : Cursor := Near.First;
            begin
               while Has_Element (I) and not Bailout loop
                  if Solution.Is_Empty or else Solution.Last_Element /= Element (I) then
                     Dive (New_Solution, Element (I));
                  end if;
                  Next (I);
               end loop;
            end;
         end;
      end Dive;

   begin
      Dive (String_Vectors.Empty_Vector, From);

      if not Bailout then
         raise Constraint_Error with "No path from " & From & " to " & To;
      end if;

--        Log ("Shortest Path:", Always);
--        for I in Result.First_Index .. Result.Last_Index loop
--           Log (Result.Element (I), Always);
--        end loop;

      return Result;
   end Depth_First_Search;

   -------------------
   -- Make_All_Weak --
   -------------------

   procedure Make_All_Weak (This : in out Object;
                            Bots :        Agpl.Containers.String_Sets.Set)
   is
      use Link_Maps;

      procedure Query (I : Cursor) is
         procedure Update (K : Key; Val : in out Link_Qualities) is
         begin
            if Bots.Contains (K.R1) or else Bots.Contains (K.R2) then
               Log ("MARKING IDLE " & K.R1 & " or " & K.R2, Never);
               Val := Link_Qualities'Max (Val,
                                          Link_Qualities'Adjacent
                                            (This.Umbral, This.Umbral * 2.0));
            end if;
         end Update;
      begin
         This.Update_Element (I, Update'Access);
      end Query;
   begin
      Log ("IDLE BOTS: " & Bots.Length'Img, Never);
      This.Iterate (Query'Access);
   end Make_All_Weak;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (This : Object; Bot : String) return Boolean is
   begin
      return Natural (This.Neighbors (Bot, Only_Weak => True).Length) <= 1;
   end Is_Leaf;

   --------------
   -- Distance --
   --------------

   function Distance (This : Object; From, To : String) return Natural is
   begin
      return Natural (This.Depth_First_Search (From, To).Length) - 1;
   end Distance;

end Sancta.Ctree.Connectivity_Matrix;
