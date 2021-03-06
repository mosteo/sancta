

with Agpl.Constants;
with Agpl.Streams.Circular_Unbounded;
with Agpl.Strings;
use Agpl;

with Ada.Containers;

package body Sancta.Plan is

   use type Ada.Containers.Count_Type;
   use type Subplan;

   Boolean_To_Char : constant array (Boolean) of Character := (True  => 'T',
                                                               False => 'f');

   ---------
   -- "=" --
   ---------

   function "=" (L, R : in Object) return Boolean is
      use type Sancta.Method.Vectors.Vector;
   begin
--        if L.Dirty /= R.Dirty then
--           Log ("Plans are of different dirtyness", Debug, Detail_Section);
--        end if;
--        if L.Methods /= R.Methods then
--           Log ("Plans are of different methodology", Debug, Detail_Section);
--        end if;
--        if not Plan_Node.Equivalent (L.Tasks, R.Tasks) then
--           Log ("Plan nodes are not equivalent", Debug, Detail_Section);
--        end if;
      return L.Dirty = R.Dirty and then
             L.Methods = R.Methods and then
             Plan_Node.Equivalent (L.Tasks, R.Tasks);
   end "=";

   -----------------
   -- Add_Subplan --
   -----------------

   procedure Add_Subplan
     (This : in out Object;
      Comp : in     Subplan;
      Kind : in     Node_Kind := Plan_Node.And_Node)
   is
      use Plan_Node;
   begin
      if This.Tasks = null then
         This.Tasks := Comp;
      elsif Get_Kind (This.Tasks) = And_Node and then Kind = And_Node then
         Plan_Node.Append_Child (This.Tasks, Comp);
      else
         This.Tasks := Plan_Node.Create (Kind, This.Tasks, Comp);
      end if;

      Plan_Node.Build_Index (This.Tasks, This.Index);
      This.Dirty := True;
   end Add_Subplan;

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (This : in out Object;
      The_Method : in Method.Object'Class)
   is
   begin
      This.Methods.Append (The_Method);
   end Add_Method;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (This : in out Object;
      The_Task : in Tasks.Object'Class;
      Kind     : in Node_Kind := Plan_Node.And_Node)
   is
      use Plan_Node;
   begin
      if This.Tasks = null then
         This.Tasks := Plan_Node.Create (The_Task);
      elsif Get_Kind (This.Tasks) = And_Node and then Kind = And_Node then
         --  Add to tail
         Plan_Node.Append_Child (This.Tasks, Plan_Node.Create (The_Task));
      else
         --  New deep
         This.Tasks := Plan_Node.Create
           (Kind,
            This.Tasks,
            Plan_Node.Create (The_Task));
      end if;

      Plan_Node.Build_Index (This.Tasks, This.Index);
      This.Dirty := True;
   end Add_Task;

   ----------------
   -- Empty_Plan --
   ----------------

   function Empty_Plan return Object is
   begin
      return (Ada.Finalization.Controlled with others => <>);
   end Empty_Plan;

   ---------------------
   -- Enumerate_Tasks --
   ---------------------

   procedure Enumerate_Tasks
     (This           : in     Object;
      Tasks          :    out Sancta.Tasks.Containers.Lists.List;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False)
   is
   begin
      Plan_Node.Enumerate_Tasks (This.Get_Root,
                                 Tasks,
                                 Compound  => Compound,
                                 Primitive => Primitive,
                                 Finished  => Finished,
                                 Pending   => Pending);
   end Enumerate_Tasks;

   ---------------------
   -- Enumerate_Tasks --
   ---------------------

   function Enumerate_Tasks
     (This           : in     Object;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False)
      return                  Sancta.Tasks.Containers.Lists.List
   is
      Result : Sancta.Tasks.Containers.Lists.List;
   begin
      Enumerate_Tasks (This,
                       Result,
                       Compound  => Compound,
                       Primitive => Primitive,
                       Finished  => Finished,
                       Pending   => Pending);

      return Result;
   end Enumerate_Tasks;

   ------------
   -- Expand --
   ------------

   procedure Expand (This    : in Object;
                     Results : not null access procedure (Found : in Object)) is

      ----------------------
      -- Recursive_Expand --
      ----------------------

      procedure Recursive_Expand
        (Node        : in     Plan_Node.Node_Access;
         Last_Branch : in     Boolean;
         Continue    :    out Boolean)
      --  Last branch is used to signal that we must report this plan once
      --  this branch is completely expanded.
      is
      begin
         --  Text_Io.Put_Line ("Recursing...");
         Continue := True;
         --  By default we continue. Will abort continuation on precise points.

         case Plan_Node.Get_Kind (Node) is
            when Plan_Node.Task_Node =>
               --  No expansion for primitive tasks
               Log
                 ("At task " & Tasks.To_String (Plan_Node.Get_Task (Node)) &
                  "; Expanded: " & Boolean'Image (Plan_Node.Get_Expanded (Node)) &
                  "; Primitive: " & Boolean'Image (Sancta.Tasks.Is_Primitive (Plan_Node.Get_Task (Node))) &
                  "; Class: " & External_Tag (Plan_Node.Get_Task (Node).all'Tag),
                  Debug,
                  Section => Detail_Section);

               if Tasks.Is_Primitive (Plan_Node.Get_Task (Node)) then
                  --  Report if plan completed.
                  if Last_Branch then
                     Log ("Reporting completed expansion", Debug,
                          Section => Detail_Section);
                     Results (This);
                  end if;
               elsif (not Plan_Node.Get_Expanded (Node)) and then
                     (not Plan_Node.Get_Finished (Node))
               then
                  --  Try all expansions on this task.
                  declare
                     use Method.Vectors;
                     I        : Cursor  := First (This.Methods);
                     Expanded : Boolean := False;
                  begin
                     while I /= No_Element loop
                        declare
                           N : constant Subplan :=
                                 Method.Apply (Element (I),
                                               Plan_Node.Get_Task (Node));
                        begin
                           --  Expand a new non-empty expansion:
                           if N /= null then
                              Expanded := True; -- At least one valid expansion.
                              declare
                                 New_Plan : Object :=
                                              Copy_But_Tasks (This);
                                 New_Node : constant Plan_Node.Node_Access :=
                                              Plan_Node.Deep_Copy (Node);
                              begin
                                 --  Hook the expansion in the copied node:
                                 Plan_Node.Set_Child (New_Node, N);

                                 pragma Assert (Plan_Node.Get_Expanded (New_Node));

                                 --  Duplicate the plan replacing the task node.
                                 Set_Tasks
                                   (New_Plan,
                                    Plan_Node.Deep_Copy_With_Replace
                                      (This.Tasks,
                                       Node,
                                       New_Node,
                                       null));
                                 Log ("Task Expanded Succesfully: " &
                                      Tasks.To_String
                                        (Plan_Node.Get_Task (Node)),
                                      Debug,
                                      Section => Detail_Section);
                                 pragma Assert (Plan_Node.Is_Sane (This.Tasks));
                                 Log ("Starting top-level expansion", Debug,
                                      Section => Detail_Section);
                                 Expand (New_Plan, Results);
                              end;
                           end if;
                        end;
                        Next (I);
                     end loop;
                     Continue := False;
                     if not Expanded then
                        Trace.Log ("Unable to expand task " &
                                   External_Tag (Plan_Node.Get_Task (Node).all'Tag),
                                   Trace.Warning);
                        raise Expansion_Failed;
                     end if;
                  end;
               else
                  --  Examine expanded childs of this task:
                  Log ("Exploring child tasks node", Debug, Section => Detail_Section);
                  pragma Assert (Node /= Plan_Node.Get_Expansion (Node));
                  Recursive_Expand
                    (Plan_Node.Get_Expansion (Node), Last_Branch, Continue);
               end if;
            --  Expand all AND nodes
            when Plan_Node.And_Node =>
               Log ("Exploring AND node", Debug, Section => Detail_Section);
               declare
                  use Plan_Node.Node_Lists;
                  Nodes : constant List := Plan_Node.Get_Children (Node);
                  I     : Cursor        := First (Nodes);
                  Cont  : Boolean       := True;
               begin
                  while I /= No_Element and then Cont loop
                     Recursive_Expand (Element (I),
                                       Last_Branch and then I = Last (Nodes),
                                       Cont);
                     Next (I);
                  end loop;
               end;
            --  Create & expand a new plan containing each one of the Or branches.
            when Plan_Node.Or_Node =>
               Log ("Exploding OR node", Debug, Section => Detail_Section);
               declare
                  use Plan_Node.Node_Lists;
                  Nodes : constant List := Plan_Node.Get_Children (Node);
                  I     : Cursor        := First (Nodes);
               begin
                  while I /= No_Element loop
                     declare
                        New_Plan : Object := Copy_But_Tasks (This);
                     begin
                        Set_Tasks
                          (New_Plan,
                           Plan_Node.Deep_Copy_With_Replace
                             (This.Tasks,
                              Node,
                              Plan_Node.Create
                                (Plan_Node.And_Node,
                                 Plan_Node.Deep_Copy (Element (I)), null), null));
                        pragma Assert (Plan_Node.Is_Sane (This.Tasks));
                        Log ("Starting new top-level expansion", Debug, Section => Detail_Section);
                        Expand (New_Plan, Results);
                     end;
                     Next (I);
                  end loop;
                  Continue := False; -- This plan has been split, no continue.
               end;
         end case;
      end Recursive_Expand;

      Continue : Boolean := True;
   begin
      --  Text_Io.Put_Line ("Expanding Plan:");
--      Print_Summary (This);
      Recursive_Expand (This.Tasks, True, Continue);
      Log ("Top-level expand finished", Debug, Section => Detail_Section);
   exception
      when Expansion_Failed =>
         Trace.Log ("Plan expansion failed",
                    Trace.Debug,
                    Section => Constants.HTN);
   end Expand;

   ------------
   -- Expand --
   ------------

   function Expand (This : in Object) return Object is

      use Plan_Node.Node_Lists;
      Solutions : List;

      ----------------
      -- Plan_Found --
      ----------------

      procedure Plan_Found (A_Plan : in Object) is
      begin
         Log ("Registering expansion", Debug, Section => Detail_Section);
         pragma Assert (Plan_Node.Is_Sane (A_Plan.Get_Root));
         Solutions.Append (Plan_Node.Deep_Copy (A_Plan.Get_Root));
      end Plan_Found;

   begin
      Expand (This, Plan_Found'Unrestricted_Access);
      Log ("Expansions finished.", Debug, Section => Detail_Section);

      if Solutions.Is_Empty then
         Log ("No plans found in expansion", Debug, Section => Detail_Section);
         return Empty_Plan;
      else
         Log ("Plan.Expand: " & Solutions.Length'Img & " expansions found", Debug, Section => Detail_Section);
         declare
            Solution : Object := Copy_But_Tasks (This);
         begin
            Add_Subplan (Solution, Plan_Node.Create (Plan_Node.Or_Node, Solutions));
            pragma Assert (Plan_Node.Is_Sane (Solution.Get_Root));
            return Solution;
         end;
      end if;
   end Expand;

   -------------------
   -- Extract_Tasks --
   -------------------

   procedure Extract_Tasks
     (This  : in Plan_Node.Node_Access;
      Tasks : out Sancta.Tasks.Containers.Lists.List) is
   begin
      Plan_Node.Enumerate_Tasks (This, Tasks,
                                 Primitive => True,
                                 Compound  => True,
                                 Pending   => True,
                                 Finished  => True);
   end Extract_Tasks;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (This : in Object;
      Id   : in Tasks.Task_Id) return Subplan
   is
      use Plan_Node.Task_Id_To_Node;
   begin
      if This.Index.Contains (Id) then
         return This.Index.Element (Id);
      else
         return null;
      end if;
   end Get_Node;

   --------------
   -- Get_Root --
   --------------

   function Get_Root (This : in Object) return Subplan is
   begin
      return This.Tasks;
   end Get_Root;

   --------------
   -- Get_Task --
   --------------

   function Get_Task
     (This : in Object;
      Id   : in Tasks.Task_Id) return Tasks.Object_Access
   is
      use Plan_Node.Task_Id_To_Node;
   begin
      return Plan_Node.Get_Task (Element (Find (This.Index, Id)));
   end Get_Task;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : in Object) return Tasks.Containers.Lists.List is
      Tasks : Sancta.Tasks.Containers.Lists.List;
   begin
      Extract_Tasks (This.Tasks, Tasks);
      return Tasks;
   end Get_Tasks;

   -------------
   -- Inflate --
   -------------

   function Inflate (This : in Object) return Object is
      Result     : Object  := This;
      Recursions : Natural := 0;

      --  Inflate_Node --
      procedure Inflate_Node (Node : in Subplan) is
         use Plan_Node;
      begin
         Recursions := Recursions + 1;
         if Node = null then
            return;
         else
            case Get_Kind (Node) is
               when Task_Node =>
                  --  Expand unexpanded compound tasks
                  if (not Sancta.Tasks.Is_Primitive (Get_Task (Node))) and then
                    (not Get_Expanded (Node))
                  then
                     declare
                        Expansions : Node_Vectors.Vector;
                     begin
                        for I in This.Methods.First_Index ..
                                 This.Methods.Last_Index
                        loop
                           declare
                              Expansion : constant Subplan :=
                                            Method.Apply
                                              (This.Methods.Element (I),
                                               Get_Task (Node));
                           begin
                              if Expansion /= null then
                                 Expansions.Append (Expansion);
                              end if;
                           end;
                        end loop;
                        if Expansions.Length > 1 then
                           --  Create OR node with the expansions
                           Set_Child (Node,
                                      Create (Or_Node, Expansions));
                        elsif Expansions.Length = 1 then
                           --  Hook expansion under the task.
                           Set_Child (Node,
                                      Node_Vectors.First_Element (Expansions));
                        else
                           Trace.Log ("Unable to expand task " &
                                      External_Tag (Plan_Node.Get_Task (Node).all'Tag),
                                      Trace.Warning);
                           raise Expansion_Failed;
                        end if;
                     end;
                  end if;
                  --  Go down the expansion:
                  Inflate_Node (Get_Expansion (Node));

               when Or_Node | And_Node =>
                  --  Simply check each child here:
                  declare
                     Children : constant Node_Vectors.Vector :=
                                  Get_Children (Node);
                  begin
                     for I in Children.First_Index .. Children.Last_Index loop
                        Inflate_Node (Children.Element (I));
                     end loop;
                  end;
            end case;
         end if;
      end Inflate_Node;

   begin
      Inflate_Node (Result.Tasks);

      Plan_Node.Build_Index (Result.Tasks, Result.Index);
      Result.Dirty := True;
      --  These two are probably unneeded, specially Build_Index which is
      --  called on Adjust.

      --  Log ("Inflate.Recursions =" & Recursions'Img, Always);
      pragma Assert (Plan_Node.Is_Sane (Result.Tasks));

      return Result;
   end Inflate;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : in Object) return Boolean is
   begin
      return This.Tasks = null;
   end Is_Empty;

   -------------------
   -- Fill_Finished --
   -------------------

   procedure Fill_Finished (This : in out Object) is
   begin
      Plan_Node.Fill_Finished (This.Tasks);
   end Fill_Finished;

   --------------------
   -- Mark_Task_Done --
   --------------------

   procedure Mark_Task_Done
     (This : in out Object;
      Id   : in     Tasks.Task_Id;
      Done : in     Boolean := True)
   is
      use Plan_Node;
   begin
      Set_Finished (This.Get_Node (Id), Finished => Done);
      if Done then
         This.Fill_Finished;
      else
         declare
            Curr : Subplan := This.Get_Node (Id);
         begin
            while Get_Parent (Curr) /= null and then Get_Finished (Get_Parent (Curr)) loop
               Curr := Get_Parent (Curr);
            end loop;
            Set_Finished (Curr, Finished => Done, Recursive => True);
            --  Note that Done is false in this case
         end;
      end if;
   end Mark_Task_Done;

   --------------------
   -- Mark_Task_Done --
   --------------------

   procedure Mark_Task_Done
     (This    : in out Object;
      Id      : in     Tasks.Task_Id;
      Results : not null access procedure (Found : in Object))
   is
      Finished_Task : constant Tasks.Object_Access := Get_Task (This, Id);
      Finished_Node : Subplan renames Get_Node (This, Id);


      ------------------------------
      -- Iterate_Methods_For_Leaf --
      ------------------------------

      procedure Iterate_Methods_For_Leaf (I : Method.Vectors.Cursor) is
         New_Node : Subplan;
      begin
         Method.Finished_Task (Method.Vectors.Element (I),
                               Finished_Task.all,
                               New_Node);

         --  If new task nodes created, replace current node with this:
         if New_Node /= null then
            declare
               New_Plan : Object := Copy_But_Tasks (This);
            begin
               Set_Tasks (New_Plan,
                          Plan_Node.Deep_Copy_With_Replace
                            (This.Tasks,
                             Finished_Node,
                             New_Node,
                             null));
               Results (New_Plan);
            end;
         end if;
      end Iterate_Methods_For_Leaf;

      ---------------
      -- Mark_Task --
      ---------------

      procedure Mark_Task (T : Tasks.Object_Access) is

         Node : Subplan renames Get_Node (This, Tasks.Get_Id (T.all));

         ---------------------
         -- Iterate_Methods --
         ---------------------

         procedure Iterate_Methods (I : Method.Vectors.Cursor) is
            Done     : Boolean;
            New_Node : Subplan;
         begin
            Method.Finished_Child (Method.Vectors.Element (I),
                                   T.all,
                                   Finished_Task.all,
                                   New_Node,
                                   Done);
            --  If premature ending, notify for this task.
            if Done then
               pragma Untested (Done);
               --  I don't know the final implications of this.
               --  This is probably broken.
               Mark_Task_Done (This, Tasks.Get_Id (T.all), Results);
            end if;

            --  If new task nodes created, add them under current task.
            if New_Node /= null then
               declare
                  Bis : constant Subplan :=
                          Plan_Node.Deep_Copy (Node);
               begin
                  pragma Assert (Node /= null);
                  pragma Assert (Bis  /= null);
                  Plan_Node.Append_Child (Bis, New_Node);
                  declare
                     New_Plan : Object := Copy_But_Tasks (This);
                  begin
                     Set_Tasks (New_Plan,
                                Plan_Node.Deep_Copy_With_Replace
                                  (This.Tasks,
                                   Node,
                                   Bis,
                                   null));
                     Results (New_Plan);
                  end;
               end;
            end if;
         end Iterate_Methods;

      begin
         pragma Assert (Tasks."/=" (T, null));
         pragma Assert (Node /= null);

         --  parent notifications:
         Method.Vectors.Iterate (This.Methods, Iterate_Methods'Access);

         if Plan_Node.Get_Parent_Task (Node) /= null then
            Mark_Task (Plan_Node.Get_Task (Plan_Node.Get_Parent_Task (Node)));
         end if;
      end Mark_Task;

   begin
      Plan_Node.Set_Finished (Finished_Node);

      --  Task itself notification:
      Method.Vectors.Iterate (This.Methods, Iterate_Methods_For_Leaf'Access);

      Mark_Task (Plan_Node.Get_Task (Plan_Node.Get_Parent_Task (Finished_Node)));

      --  Here we must apply each method to each task while going up, and for
      --  every modification create a new plan to be notified to caller via
      --  Results.
      --  For going up I mean traverse tasks from child to parent starting at
      --  the one being marked done.
   end Mark_Task_Done;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This : in out Object; Root : in Subplan) is
   begin
      if This.Tasks /= null then
         Plan_Node.Delete (This.Tasks);
      end if;
      This.Tasks := Root;
      Plan_Node.Build_Index (Root, This.Index);
   end Set_Tasks;

   --------------------
   -- Set_Task_Owner --
   --------------------

   procedure Set_Task_Owner
     (This  : in out Object;
      T     : in     Tasks.Task_Id;
      Owner : in     String) is
   begin
      Plan_Node.Set_Owner (Get_Node (This, T), Owner);
   end Set_Task_Owner;

   -----------------
   -- Build_Index --
   -----------------

   procedure Build_Index (This : in out Object) is
   begin
      Plan_Node.Build_Index (This.Tasks, This.Index);
   end Build_Index;

   procedure Clear (This : in out Object) is
   begin
      This.Finalize;
      This.Index.Clear;
      This.Methods.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object; Id : in Tasks.Task_Id) return Boolean is
   begin
      return This.Index.Contains (Id);
   end Contains;

   --------------------
   -- Copy_But_Tasks --
   --------------------

   function Copy_But_Tasks (This : in Object) return Object is
      New_Plan : Object;
   begin
      New_Plan.Methods := This.Methods;
      return New_Plan;
   end Copy_But_Tasks;

   -----------------
   -- Is_Modified --
   -----------------

   function Is_Modified (This : in Object) return Boolean is
   begin
      return This.Dirty;
   end Is_Modified;

   --------------------
   -- Set_Unmodified --
   --------------------

   procedure Set_Unmodified (This : in out Object) is
   begin
      This.Dirty := False;
   end Set_Unmodified;

   -------------------
   -- Print_Summary --
   -------------------

   procedure Print_Summary (This : in Object) is
      Tasks : constant Sancta.Tasks.Containers.Lists.List := Get_Tasks (This);
      use Sancta.Tasks.Containers.Lists;
      I     : Cursor := First (Tasks);
   begin
      Trace.Log ("Plan summary follows: ", Trace.Always);
      while I /= No_Element loop
         declare
            The_Task : Sancta.Tasks.Object'Class renames Element (I);
         begin
            Trace.Log (Sancta.Tasks.To_String (The_Task) & ": " &
                       External_Tag (The_Task'Tag) & "; Primitive: " &
                       Boolean'Image (Sancta.Tasks.Is_Primitive (The_Task)),
                       Trace.Always);
         end;
         Next (I);
      end loop;
   end Print_Summary;

   ------------------------
   -- Print_Tree_Summary --
   ------------------------

   procedure Print_Tree_Summary (This : in Object) is

      --  Print_Node --
      procedure Print_Node (X      : in Plan_Node.Node_Access;
                            Indent : in Natural) is
         use Plan_Node;
         Indent_Grow: constant Natural := 3;
         Whites     :          String (1 .. Indent) := (others => ' ');
         New_Indent : constant Natural := Indent + Indent_Grow;

         --  Print_Children --
         procedure Print_Children (L : in Node_Lists.List) is
            use Node_Lists;
            I : Cursor := First (L);
         begin
            while Has_Element (I) loop
               Print_Node (Element (I), New_Indent);
               Next (I);
            end loop;
         end Print_Children;

      begin
         declare I : Positive := Whites'First + 1;
         begin
            while I <= Whites'Last loop
               Whites (I) := '|';
               I := I + Indent_Grow;
            end loop;
         end;

         if X = null then
            Log (Whites & "NULL", Always);
         else
            case Get_Kind (X) is
               when And_Node =>
                  Log (Whites & "AND [" & Get_Id (X) & "]" &
                       " F:" & Boolean_To_Char (Get_Finished (X)),
                       Always);
                  Print_Children (Get_Children (X));
               when Or_Node =>
                  Log (Whites & "OR [" & Get_Id (X) & "]" &
                       " F:" & Boolean_To_Char (Get_Finished (X)),
                       Always);
                  Print_Children (Get_Children (X));
               when Task_Node =>
                  declare
                     Job : Sancta.Tasks.Object_Access renames Get_Task (X);
                  begin
                     Log (Whites &
                          "P:" & Boolean_To_Char (Sancta.Tasks.Is_Primitive (Job.all)) &
                          " E:" & Boolean_To_Char (Get_Expanded (X)) &
                          " F:" & Boolean_To_Char (Get_Finished (X)) &
                          " | " &
                          "[" & Get_Id (X) & "]" &
                          "[" & Strings.To_String (Integer (Sancta.Tasks.Get_Id (Job.all))) & "] " &
                          Sancta.Tasks.To_String (Job.all) &
                          External_Tag (Job'Tag),
                          Always);
                     if Get_Expanded (X) then
                        Print_Node (Get_Expansion (X), New_Indent);
                     end if;
                  end;
            end case;
         end if;
      end Print_Node;

   begin
      Log ("Plan tree:", Always);
      Print_Node (Get_Root (This), Indent => 0);
   end Print_Tree_Summary;

   -------------------
   -- Size_In_Bytes --
   -------------------

   function Size_In_Bytes (This : in Object) return Natural is
      Stream : aliased Streams.Circular_Unbounded.Stream_Type;
   begin
      Stream.Create;
      Object'Output (Stream'Access, This);
      return Stream.Available_read;
   end Size_In_Bytes;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Object) is
   begin
      This.Tasks := Plan_Node.Deep_Copy (This.Tasks);
      Plan_Node.Build_Index (This.Tasks, This.Index);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      Plan_Node.Delete (This.Tasks); -- Will force chain destruction.
   end Finalize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Object)
   is
   begin
      This.Dirty   := Boolean'Input (Stream);
      This.Methods := Method.Vectors.Vector'Input (Stream);
      This.Tasks   := Subplan'Input (Stream);

      This.Build_Index;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   : in     Object)
   is
   begin
      Boolean'Output (Stream, This.Dirty);
      Method.Vectors.Vector'Output (Stream, This.Methods);
      Subplan'Output (Stream, This.Tasks);
   end Write;

end Sancta.Plan;
