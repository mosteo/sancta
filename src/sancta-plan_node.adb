

with Agpl.Debug;
with Agpl.Sequence;
use Agpl;

with Ada.Containers;
with Interfaces;

package body Sancta.Plan_Node is

   package Unsigned_Sequences is new Sequence (Interfaces.Unsigned_32);

   Seq : Unsigned_Sequences.Object;

   ---------
   -- "=" --
   ---------

   function Equivalent (L, R : in Node_Access) return Boolean is
      use type Ada.Containers.Count_Type;
      use type Ustring;
      use type Sancta.Tasks.Object'Class;
      use type Node_Vectors.Vector;
   begin
      Log ("Checking equivalence", Trace.Debug, Detail_Section);
      if L = null and then R = null then
         Log ("Both null", Trace.Debug, Detail_Section);
         return True;
      elsif L = null or else R = null then
         Log ("Null/not null", Trace.Debug, Detail_Section);
         return False;
      elsif L.Kind /= R.Kind then
         Log ("Kind clash", Trace.Debug, Detail_Section);
         return False;
      elsif L.Id /= R.Id then
         Log ("Id clash", Trace.Debug, Detail_Section);
         return False;
      end if;

      case L.Kind is
         when Task_Node =>
            if L.The_Task.all /= R.The_Task.all or else
              L.Finished /= R.Finished or else
              L.Owner /= R.Owner
            then
               Log ("Task clash", Trace.Debug, Detail_Section);
               return False;
            else
               Log ("Expansion check", Trace.Debug, Detail_Section);
               return Equivalent (L.Child, R.Child);
            end if;
         when And_Node | Or_Node =>
            if L.Children.Length /= R.Children.Length then
               Log ("Children length clash", Trace.Debug, Detail_Section);
               return False;
            elsif L.Children.First_Index /= R.Children.First_Index then
               Log ("Index clash", Trace.Debug, Detail_Section);
               return False;
            else
               for I in L.Children.First_Index .. L.Children.Last_Index loop
                  if not Equivalent (L.Children.Element (I),
                                     R.Children.Element (I)) then
                     Log ("Child clash", Trace.Debug, Detail_Section);
                     return False;
                  end if;
               end loop;
               Log ("Children are ok", Trace.Debug, Detail_Section);
               return True;
            end if;
      end case;
   end Equivalent;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child (This, Child : Node_Access) is
   begin
      pragma Assert (This /= null);
      pragma Assert (Child /= null);

      case This.Kind is
         when Task_Node =>
            pragma Assert (This.Child /= null);
            Append_Child (This.Child, Child);
         when And_Node | Or_Node =>
            Node_Vectors.Append (This.Children, Child);
      end case;
   end Append_Child;

   -----------------
   -- Build_Index --
   -----------------

   procedure Build_Index
     (Node  : in     Node_Access;
      Index : in out Task_Id_To_Node.Map;
      Clean : in     Boolean := True)
   is
      use Task_Id_To_Node;
   begin
      if Clean then
         Clear (Index);
      end if;

      if Node = null then
         return;
      else
         case Node.Kind is
            when Task_Node =>
               Include (Index, Tasks.Get_Id (Node.The_Task.all), Node);
               Build_Index (Node.Child, Index, False);
            when And_Node | Or_Node =>
               for I in Node.Children.First_Index .. Node.Children.Last_Index loop
                  Build_Index (Node_Vectors.Element (Node.Children, I),
                               Index,
                               False);
               end loop;
         end case;
      end if;
   end Build_Index;

   ------------
   -- Create --
   ------------

   function Create (From   : in Tasks.Object'Class;
                    Parent : in Node_Access := null) return Node_Access is
      Node : constant Node_Access := new Object (Task_Node);
   begin
      Node.Parent   := Parent;
      Node.The_Task := new Tasks.Object'Class'(From);

      return Node;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kind   : in Composite_Node_Kind;
      From   : in Tasks.Containers.Lists.List;
      Parent : in Node_Access := null) return Node_Access
   is
      use Sancta.Tasks.Containers.Lists;
      I    : Cursor               := First (From);
      Node : constant Node_Access := new Object (Kind);
   begin
      Node.Parent := Parent;
      while I /= No_Element loop
         Node_Vectors.Append (Node.Children, Create (Element (I), Node));
         Next (I);
      end loop;

      return Node;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kind   : in Composite_Node_Kind;
      L, R   : in Node_Access;
      Parent : in Node_Access := null) return Node_Access
   is
      New_Node : constant Node_Access := new Object (Kind);
   begin
      New_Node.Parent := Parent;
      if L /= null then
         L.Parent        := New_Node;
         Node_Vectors.Append (New_Node.Children, L);
      end if;
      if R /= null then
         R.Parent        := New_Node;
         Node_Vectors.Append (New_Node.Children, R);
      end if;

      return New_Node;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Lists.List;
      Parent : in Node_Access := null) return Node_Access
   is
      Fresh : constant Node_Access := new Object (Kind);
      use Node_Lists;
      I     : Cursor := First (Nodes);
   begin
      while Has_Element (I) loop
         Fresh.Children.Append (Element (I));
         Next (I);
      end loop;

      Fresh.Parent   := Parent;

      I := First (Nodes);
      while I /= No_Element loop
         Element (I).Parent := Fresh;
         Next (I);
      end loop;

      return Fresh;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Vectors.Vector;
      Parent : in Node_Access := null) return Node_Access
   is
      Fresh : constant Node_Access := new Object (Kind);
   begin
      Fresh.Children := Nodes;
      Fresh.Parent   := Parent;

      for I in Nodes.First_Index .. Nodes.Last_Index loop
         Node_Vectors.Element (Nodes, I).Parent := Fresh;
      end loop;

      return Fresh;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Kind   : in Composite_Node_Kind;
      Nodes  : in Node_Array;
      Parent : in Node_Access := null) return Node_Access
   is
      Fresh : constant Node_Access := new Object (Kind);
   begin
      Fresh.Parent := Parent;
      for I in Nodes'Range loop
         Nodes (I).Parent := Fresh;
         Node_Vectors.Append (Fresh.Children, Nodes (I));
      end loop;
      return Fresh;
   end Create;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (This   : in Node_Access;
                       Parent : in Node_Access := null) return Node_Access is
   begin
      return Deep_Copy_With_Replace (This, null, null, Parent);
   end Deep_Copy;

   ----------------------------
   -- Deep_Copy_With_Replace --
   ----------------------------

   function Deep_Copy_With_Replace
     (This     : in Node_Access;
      Old_Node : in Node_Access;
      New_Node : in Node_Access;
      Parent   : in Node_Access) return Node_Access is
   begin
      if This = null then
         return null;
      elsif This = Old_Node then
         New_Node.Parent := Parent;
         return New_Node;
      else
         declare
            Fresh : constant Node_Access := new Object (This.Kind);
            use type Interfaces.Unsigned_32;
         begin
            Fresh.Id     := This.Id;
            Seq.Set_Next (Seq.Peek_Next - 1); -- Trick not to waste ids...

            Fresh.Parent := Parent;
            if This.Kind = Task_Node then
               Set_Child (Fresh,
                          Deep_Copy_With_Replace
                            (This.Child, Old_Node, New_Node, Fresh));
               Fresh.The_Task := new Tasks.Object'Class'(This.The_Task.all);
               Fresh.Finished := This.Finished;
               Fresh.Owner    := This.Owner;
               return Fresh;
            else
               declare
                  use Node_Vectors;
                  I        : Cursor := First (This.Children);
                  New_List : Vector;
               begin
                  while I /= No_Element loop
                     Append (New_List,
                             Deep_Copy_With_Replace
                               (Element (I), Old_Node, New_Node, Fresh));
                     Next (I);
                  end loop;
                  Fresh.Children := New_List;
                  return Fresh;
               end;
            end if;
         end;
      end if;
   end Deep_Copy_With_Replace;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Node_Access) is
   begin
      Delete_Internal (This);
   end Delete;

   ---------------------
   -- Enumerate_Tasks --
   ---------------------

   procedure Enumerate_Tasks
     (This           : in     Node_Access;
      Tasks          :    out Sancta.Tasks.Containers.Lists.List;
      Compound       : in     Boolean := False;
      Primitive      : in     Boolean := False;
      Finished       : in     Boolean := False;
      Pending        : in     Boolean := False)
   is
      use Node_Vectors;
   begin
      if This = null then
         return; -- Nothing to do!
      elsif This.Kind = Task_Node then
         if ((Compound and then not Sancta.Tasks.Is_Primitive (This.The_Task.all)) or else
               (Primitive and then Sancta.Tasks.Is_Primitive (This.The_Task.all))) and then
           ((Finished and then This.Finished) or else
              (Pending and then not This.Finished))
         then
            Tasks.Append (This.The_Task.all);
         end if;
         if Get_Expanded (This) then
            Enumerate_Tasks (Node_Access (This.Child), Tasks,
                             Compound, Primitive,
                             Finished, Pending);
         end if;
      else
         for I in This.Children.First_Index .. This.Children.Last_Index loop
            Enumerate_Tasks (Node_Access (Element (This.Children, I)),
                             Tasks,
                             Compound, Primitive,
                             Finished, Pending);
         end loop;
      end if;
   end Enumerate_Tasks;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (This : in Node_Access) return Node_Kind is
   begin
      return This.Kind;
   end Get_Kind;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (This : in Node_Access) return Node_Lists.List is
      Result : Node_Lists.List;
   begin
      for I in This.Children.First_Index .. This.Children.Last_Index loop
         Result.Append (This.Children.Element (I));
      end loop;

      return Result;
   end Get_Children;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (This : in Node_Access) return Node_Vectors.Vector is
   begin
      return This.Children;
   end Get_Children;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded (This : in Node_Access) return Boolean is
   begin
      if This.Kind /= Task_Node then
         raise Constraint_Error;
      end if;

      return This.Child /= null;
   end Get_Expanded;

   -------------------
   -- Get_Expansion --
   -------------------

   function Get_Expansion (This : in Node_Access) return Node_Access is
   begin
      return This.Child;
   end Get_Expansion;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : not null Node_Access) return String is
   begin
      return +This.Id;
   end Get_Id;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner (This : in Node_Access) return String is
   begin
      return +This.Owner;
   end Get_Owner;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner (This : in Node_Access; Owner : in String) is
   begin
      This.Owner := +Owner;
   end Set_Owner;

   --------------
   -- Get_Task --
   --------------

   function Get_Task (This : in Node_Access) return Tasks.Object'Class is
   begin
      return This.The_Task.all;
   end Get_Task;

   --------------
   -- Get_Task --
   --------------

   function Get_Task (This : in Node_Access) return Tasks.Object_Access is
   begin
      return This.The_Task;
   end Get_Task;

   ------------------
   -- Get_Finished --
   ------------------

   function Get_Finished (This : Node_Access) return Boolean is
   begin
      return This.Finished;
   end Get_Finished;

   ------------------
   -- Set_Finished --
   ------------------

   procedure Set_Finished (This      : Node_Access;
                           Finished  : Boolean := True;
                           Recursive : Boolean := False) is
   begin
      This.Finished := Finished;

      if Recursive then
         case This.Kind is
         when Task_Node =>
            if Get_Expanded (This) then
               Set_Finished (Get_Expansion (This), Finished, Recursive);
            end if;
         when Or_Node | And_Node  =>
            declare
               Children : constant Node_Vectors.Vector := Get_Children (This);
            begin
               for I in Children.First_Index .. Children.Last_Index loop
                  Set_Finished (Children.Element (I), Finished, Recursive);
               end loop;
            end;
         end case;
      end if;
   end Set_Finished;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child (This  : in Node_Access;
                        Child : in Node_Access;
                        Force : in Boolean := False) is
   begin
      pragma Assert (This.Child = null or else Force);

      This.Child := Child;
      if Child /= null then
         Child.Parent  := This;
      end if;
   end Set_Child;

   procedure Set_Children (This     : in Node_Access;
                           Children : in Node_Vectors.Vector;
                           Force    : in Boolean := False)
   is
   begin
      if (not Force) and then Natural (This.Children.Length) > 0 then
         raise Constraint_Error;
      end if;

      This.Children := Children;
      for I in This.Children.First_Index .. This.Children.Last_Index loop
         declare
            procedure Modify (X : in out Node_Access) is
            begin
               X.Parent := This;
            end Modify;
         begin
            This.Children.Update_Element (I, Modify'Access);
         end;
      end loop;
   end Set_Children;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (This : in Node_Access) return Node_Access is
   begin
      return This.Parent;
   end Get_Parent;

   ---------------------
   -- Get_Parent_Task --
   ---------------------

   function Get_Parent_Task (This : in Node_Access) return Node_Access is
      Curr : Node_Access := Get_Parent (This);
   begin
      while Curr /= null loop
         if Curr.Kind = Task_Node then
            return Curr;
         end if;

         Curr := Curr.Parent;
      end loop;

      return Curr; -- Will be null.
   end Get_Parent_Task;

   -------------------
   -- Fill_Finished --
   -------------------

   procedure Fill_Finished (This : in Node_Access) is
   begin
      case This.Kind is
         when Or_Node =>
            declare
               Children : constant Node_Vectors.Vector := Get_Children (This);
               Finished :          Boolean := False;
            begin
               for I in Children.First_Index .. Children.Last_Index loop
                  Fill_Finished (Children.Element (I));
                  Finished := Finished or Get_Finished (Children.Element (I));
               end loop;
               if Finished then
                  Set_Finished (This, Finished, Recursive => True);
               end if;
            end;
         when And_Node =>
            declare
               Children : constant Node_Vectors.Vector := Get_Children (This);
               Finished :          Boolean := True;
            begin
               for I in Children.First_Index .. Children.Last_Index loop
                  Fill_Finished (Children.Element (I));
                  Finished := Finished and Get_Finished (Children.Element (I));
               end loop;
               Set_Finished (This, Finished, Recursive => False);
            end;
         when Task_Node =>
            if Get_Expanded (This) then
               Fill_Finished (Get_Expansion (This));
               Set_Finished (This, Get_Finished (Get_Expansion (This)));
            end if;
      end case;
   end Fill_Finished;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object) is
      Next : Interfaces.Unsigned_32;
   begin
      Seq.Get_Next (Next);
      This.Id := + Interfaces.Unsigned_32'Image (Next);
   end Initialize;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (This    : in Node_Access;
                         Of_This : in Node_Access) return Boolean
   is
      Current : Node_Access := Of_This;
   begin
      while Current /= null loop
         if Current = This then
            return True;
         end if;
         Current := Get_Parent (Current);
      end loop;
      return False;
   end Is_Ancestor;

   -------------
   -- Is_Sane --
   -------------

   function Is_Sane (This   : in Node_Access;
                     Parent : in Node_Access := null) return Boolean
   is
   begin
      if This = null then
         return True;
      end if;

      case This.Kind is
         when Task_Node =>
            return Node_Access (This.Parent) = Parent and then
                   Is_Sane (Node_Access (This.Child), This);
         when And_Node | Or_Node =>
            declare
               Ok : Boolean := True;
            begin
               for I in This.Children.First_Index .. This.Children.Last_Index loop
                  Ok := Ok and Is_Sane (This.Children.Element (I),
                                        This);
                  exit when not Ok;
               end loop;
               return Node_Access (This.Parent) = Parent and then Ok;
            end;
      end case;
   end Is_Sane;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      case This.Kind is
         when Task_Node =>
            Tasks.Delete (This.The_Task);
            Delete (This.Child);
         when And_Node | Or_Node =>
            declare
               A : Node_Access;
            begin
               for I in This.Children.First_Index .. This.Children.Last_Index loop
                  A := This.Children.Element (I);
                  Delete (A);
               end loop;
            end;
      end case;
   exception
      when E : others =>
         Trace.Log ("Plan_Node Finalize exception: " & Agpl.Debug.Report (E),
                    Trace.Error);
          raise;
   end Finalize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Node_Access;
      Parent : in     Node_Access)
   is
      Not_Null : constant Boolean := Boolean'Input (Stream);
   begin
      if Not_Null then
         declare
            Kind : constant Node_Kind := Node_Kind'Input (Stream);
         begin
            This        := new Object (Kind);
            This.Parent := Parent;
            This.Id     := Ustring'Input (Stream);
            case This.Kind is
               when Task_Node =>
                  This.The_Task := Tasks.Object_Access'Input (Stream);
                  This.Finished := Boolean'Input (Stream);
                  This.Owner    := Ustring'Input (Stream);
                  Read (Stream, This.Child, This);
               when And_Node | Or_Node =>
                  declare
                     Num_Children : constant Natural := Natural'Input (Stream);
                     New_Child    :          Node_Access;
                  begin
                     for I in 1 .. Num_Children loop
                        Read (Stream, New_Child, This);
                        This.Children.Append (New_Child);
                     end loop;
                  end;
            end case;
         end;
      else
         This := null;
      end if;
   end Read;

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Node_Access)
   is
   begin
      Read (Stream, This, null);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   : in     Node_Access)
   is
   begin
      Boolean'Output (Stream, This /= null);
      if This = null then
         return;
      end if;

      Node_Kind'Output (Stream, This.Kind);
      Ustring'Output (Stream, This.Id);
      case This.Kind is
         when Task_Node =>
            Tasks.Object_Access'Output (Stream, This.The_Task);
            Boolean'Output (Stream, This.Finished);
            Ustring'Output (Stream, This.Owner);
            Write (Stream, This.Child);
         when And_Node | Or_Node =>
            Natural'Output (Stream, Natural (This.Children.Length));
            for I in This.Children.First_Index .. This.Children.Last_Index loop
               Write (Stream, This.Children.Element (I));
            end loop;
      end case;
   end Write;

end Sancta.Plan_Node;
