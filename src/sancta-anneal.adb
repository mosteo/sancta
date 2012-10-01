with Sancta.Methods.Choose_Entry_Point;
with Sancta.Methods.Explore_Segment_Expansion;
with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Entry_Point;
with Sancta.Tasks.Entry_Point.Flip;
with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Explore_Segment;
with Sancta.Tasks.Explore_Directed_Edge;
with Sancta.Tasks.Explore_Edge;

with Sancta.Plan_Node;
with Agpl.Random;
with Agpl; use Agpl;

package body Sancta.Anneal is

   ----------
   -- Flip --
   ----------

   function Flip
     (This : in Sancta.Tasks.Primitive.Object'Class)
      return Sancta.Tasks.Primitive.Object'Class
   is
   begin
      if This in Tasks.Explore_Directed_Segment.Object then
         declare
            use Tasks.Explore_Directed_Segment;
         begin
            return Tasks.Explore_Directed_Segment.Flip
              (Tasks.Explore_Directed_Segment.Object (This));
         end;
      elsif This in Tasks.Explore_Directed_Edge.Object then
         declare
            use Tasks.Explore_Directed_Edge;
         begin
            return Tasks.Explore_Directed_Edge.Flip
              (Tasks.Explore_Directed_Edge.Object (This));
         end;
      elsif This in Tasks.Entry_Point.Object then
         return Tasks.Entry_Point.Flip (Tasks.Entry_Point.Object (This));
      else
         return This;
      end if;
   end Flip;

   ----------------------
   -- Get_All_Children --
   ----------------------

   function Get_All_Children (This : in Sancta.Tasks.Object'Class)
                              return Sancta.Tasks.Containers.Vectors.Vector
   is
      Result   : Sancta.Tasks.Containers.Vectors.Vector;

      ------------------
      -- Build_Vector --
      ------------------

      procedure Build_Vector (Node : Sancta.Plan_Node.Node_Access) is
         Children : constant Sancta.Plan_Node.Node_Vectors.Vector :=
                      Sancta.Plan_Node.Get_Children (Node);
      begin
         for I in Children.First_Index .. Children.Last_Index loop
            Result.Append (Sancta.Plan_Node.Get_Task (Children.Element (I)));
         end loop;
      end Build_Vector;
   begin
      if This.Is_Primitive then
         Result.Append (This);
         return Result;
      end if;

      if This in Tasks.Explore_Segment.Object or else
         This in Tasks.Explore_Edge.Object
      then
         declare
            Expander : Methods.Explore_Segment_Expansion.Object;
            Expanded : Sancta.Plan_Node.Node_Access := Expander.Apply (This);
         begin
            Build_Vector (Expanded);
            Sancta.Plan_Node.Delete (Expanded); -- Avoid mem leak.
            return Result;
         end;
      elsif This in Tasks.Choose_Entry_Point.Object then
         declare
            Expander : Methods.Choose_Entry_Point.Object;
            Expanded : Sancta.Plan_Node.Node_Access := Expander.Apply (This);
         begin
            Build_Vector (Expanded);
            Sancta.Plan_Node.Delete (Expanded); -- Avoid mem leak.
            return Result;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_All_Children;

   -------------------
   -- Get_Any_Child --
   -------------------

   function Get_Any_Child (This : in Sancta.Tasks.Object'Class)
                           return    Sancta.Tasks.Primitive.Object'Class
   is
      Children : constant Sancta.Tasks.Containers.Vectors.Vector :=
                   Get_All_Children (This);
      use Sancta.Tasks.Containers.Vectors;
   begin
      return Sancta.Tasks.Primitive.Object'Class
        (Children.Element (Random.Get_Integer (First_Index (Children),
                                               Last_Index  (Children))));
   end Get_Any_Child;

   ------------
   -- Parent --
   ------------

   function Parent (This : in Sancta.Tasks.Primitive.Object'Class)
                    return    Sancta.Tasks.Object'Class
   is
   begin
      if This in Tasks.Explore_Directed_Segment.Object then
         declare
            T : Tasks.Explore_Directed_Segment.Object renames
              Tasks.Explore_Directed_Segment.Object (This);
            use Tasks.Explore_Directed_Segment;
         begin
            return Tasks.Explore_Segment.Create (Get_From (T), Get_To (T));
         end;
      elsif This in Tasks.Explore_Directed_Edge.Object then
         declare
            T : Tasks.Explore_Directed_Edge.Object renames
              Tasks.Explore_Directed_Edge.Object (This);
            use Tasks.Explore_Directed_Edge;
         begin
            return Tasks.Explore_Edge.Create (Get_From (T),
                                              Get_To (T));
         end;
      elsif This in Tasks.Entry_Point.Object then
         declare
            T : Tasks.Entry_Point.Object renames
              Tasks.Entry_Point.Object (This);
            use Tasks.Entry_Point;
         begin
            return Tasks.Choose_Entry_Point.Create (Candidates (T));
         end;
      else
         return This;
      end if;
   end Parent;

end Sancta.Anneal;
