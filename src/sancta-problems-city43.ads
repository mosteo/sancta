with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Explore_Edge;
with Sancta.Types; use Sancta.Types;

with Sancta.Tasks.Containers;

package Sancta.Problems.City43 is

   --  "With"ing this unit will cause the computation of the 43-streets problem
   --  for the Explore_Edge graph during elaboration.

   pragma Elaborate_Body;

   function Get_Tasks return Sancta.Tasks.Containers.Vectors.Vector;
   --  Get a vector with Explore_Edge tasks for the graph.

   function Get_Entry_Vertex (Pose : Types.Pose) return
     Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;
   --  Get the closer entry point vertex to a non-vertex location.
   --  Useful to get the true cost of entering the maze when the
   --  first task is not the closer one.

   Entries : constant array (1 .. 8) of Tasks.Choose_Entry_Point.Object :=
         (others =>
                Tasks.Choose_Entry_Point.Create
            ((1 => (-17.0, 16.0, -0.5),
              2 => (-24.0, -6.0,  0.0),
              3 => (  0.0,-18.0,  1.6),
              4 => ( 21.0,-18.0,  2.0),
              5 => ( 21.0,  0.0,  3.1),
              6 => ( 21.0, 19.0,  4.0),
              7 => (  0.0, 17.0,  4.6))));

end Sancta.Problems.City43;
