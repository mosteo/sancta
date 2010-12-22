with Sancta.Tasks.Explore_Segment;
with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Explore_Edge;
with Sancta.Tasks.Explore_Directed_Edge;

with Sancta.Tasks.Containers;

package body Sancta.Methods.Explore_Segment_Expansion is

   -----------
   -- Apply --
   -----------

   function Apply
     (This : in Object;
      That : in Sancta.Tasks.Object'Class)
      return Result
   is
      pragma Unreferenced (This);
   begin
      if That in Tasks.Explore_Segment.Object then
         --  Create an OR node with the two possible senses.
         declare
            Job      : Tasks.Explore_Segment.Object renames
              Tasks.Explore_Segment.Object (That);
            P1       : constant Types.Pose := Job.Get_From;
            P2       : constant Types.Pose := Job.Get_To;
            Children : Sancta.Tasks.Containers.Lists.List;
         begin
            Children.Append (Tasks.Explore_Directed_Segment.Create (P1, P2));
            Children.Append (Tasks.Explore_Directed_Segment.Create (P2, P1));

            return Sancta.Plan_Node.Create (Sancta.Plan_Node.Or_Node, Children);
         end;
      elsif That in Tasks.Explore_Edge.Object then
         declare
            Job      : Tasks.Explore_Edge.Object renames
              Tasks.Explore_Edge.Object (That);
            P1       : constant Tasks.Explore_Edge.Pose_Graphs.Vertex_Index :=
                         Job.Get_From;
            P2       : constant Tasks.Explore_Edge.Pose_Graphs.Vertex_Index :=
                         Job.Get_To;
            Children : Sancta.Tasks.Containers.Lists.List;
         begin
            Children.Append (Tasks.Explore_Directed_Edge.Create (P1, P2));
            Children.Append (Tasks.Explore_Directed_Edge.Create (P2, P1));

            return Sancta.Plan_Node.Create (Sancta.Plan_Node.Or_Node, Children);
         end;
      else
         return null;
      end if;
   end Apply;

end Sancta.Methods.Explore_Segment_Expansion;
