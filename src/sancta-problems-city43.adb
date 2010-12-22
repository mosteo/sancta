with Sancta.Types.Operations;

pragma Elaborate_All (Sancta.Types.Operations);

package body Sancta.Problems.City43 is

   G : Tasks.Explore_Edge.Pose_Graphs.Graph renames
     Tasks.Explore_Edge.Graph;

   T : Sancta.Tasks.Containers.Vectors.Vector;

   type Entry_Vertex is record
      Pose  : Types.Pose;
      Index : Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;
   end record;

   Entry_Vertices : constant array (1 .. 7) of Entry_Vertex :=
                      (1 => ((-17.0, 16.0, -0.5),  1),
                       2 => ((-24.0, -6.0,  0.0), 13),
                       3 => ((  0.0,-18.0,  1.6), 25),
                       4 => (( 21.0,-18.0,  2.0), 27),
                       5 => (( 21.0,  0.0,  3.1), 12),
                       6 => (( 21.0, 19.0,  4.0),  3),
                       7 => ((  0.0, 17.0,  4.6),  2));

   ----------------------
   -- Get_Entry_Vertex --
   ----------------------

   function Get_Entry_Vertex (Pose : Types.Pose) return
     Tasks.Explore_Edge.Pose_Graphs.Vertex_Index
   is
      use Sancta.Types.Operations;
      Best_Dist : Types.Real := Types.Real'Last;
      Best_Pos  : Integer    := 0;
   begin
      for I in Entry_Vertices'Range loop
         if Distance (Pose, Entry_Vertices (I).Pose) < Best_Dist then
            Best_Dist := Distance (Pose, Entry_Vertices (I).Pose);
            Best_Pos  := I;
         end if;
      end loop;
      return Entry_Vertices (Best_Pos).Index;
   end Get_Entry_Vertex;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks return Sancta.Tasks.Containers.Vectors.Vector is
   begin
      return T;
   end Get_Tasks;

   ---------------
   -- Add_Tasks --
   ---------------

   procedure Add_Tasks is
   begin
      --  Greate the cost vector
      declare
         use Tasks.Explore_Edge;
         use Tasks.Explore_Edge.Pose_Graphs;

         function Cost (V1, V2 : Vertex_Index) return Integer is
            use Sancta.Types.Operations;
         begin
            return Integer (Float (Time (Get_Vertex (G, V1).Data,
                                         Get_Vertex (G, V2).Data,
                                         0.5, 0.5)) *
                              Tasks.Explore_Edge.Cost_Adjust);
         end Cost;
      begin
         G.Add_Vertex (( 1, (-17.0, 15.0, 0.0)));
         G.Add_Vertex (( 2, (  0.0, 16.0, 0.0)));
         G.Add_Vertex (( 3, ( 20.0, 18.0, 0.0)));
         G.Add_Vertex (( 4, ( -8.0, 12.0, 0.0)));
         G.Add_Vertex (( 5, (  0.0, 10.0, 0.0)));
         G.Add_Vertex (( 6, ( 10.0, 10.0, 0.0)));
         G.Add_Vertex (( 7, ( 20.0, 10.0, 0.0)));
         G.Add_Vertex (( 8, (-20.0,  5.0, 0.0)));
         G.Add_Vertex (( 9, (-12.0,  3.0, 0.0)));
         G.Add_Vertex ((10, (  0.0,  5.0, 0.0)));
         G.Add_Vertex ((11, ( 10.0,  4.0, 0.0)));
         G.Add_Vertex ((12, ( 20.0,  0.0, 0.0)));
         G.Add_Vertex ((13, (-23.0, -6.0, 0.0)));
         G.Add_Vertex ((14, ( -7.0, -5.0, 0.0)));
         G.Add_Vertex ((15, (  0.0, -3.0, 0.0)));
         G.Add_Vertex ((16, (-10.0,-10.0, 0.0)));
         G.Add_Vertex ((17, ( -8.0,-10.0, 0.0)));
         G.Add_Vertex ((18, ( -3.0,-10.0, 0.0)));
         G.Add_Vertex ((19, (  0.0,-10.0, 0.0)));
         G.Add_Vertex ((20, ( 10.0, -7.0, 0.0)));
         G.Add_Vertex ((21, ( 20.0,-10.0, 0.0)));
         G.Add_Vertex ((22, ( -8.0,-15.0, 0.0)));
         G.Add_Vertex ((23, ( -3.0,-15.0, 0.0)));
         G.Add_Vertex ((24, (-13.0,-17.0, 0.0)));
         G.Add_Vertex ((25, (  0.0,-17.0, 0.0)));
         G.Add_Vertex ((26, ( 10.0,-17.0, 0.0)));
         G.Add_Vertex ((27, ( 20.0,-17.0, 0.0)));

         --  Create tasks
         T.Append (Tasks.Explore_Edge.Create ( 1,  2)); -- 1
         T.Append (Tasks.Explore_Edge.Create ( 1,  4));
         T.Append (Tasks.Explore_Edge.Create ( 1,  8));
         T.Append (Tasks.Explore_Edge.Create ( 2,  3));
         T.Append (Tasks.Explore_Edge.Create ( 3,  5)); -- 5
         T.Append (Tasks.Explore_Edge.Create ( 3,  6));
         T.Append (Tasks.Explore_Edge.Create ( 3,  7));
         T.Append (Tasks.Explore_Edge.Create ( 4,  5));
         T.Append (Tasks.Explore_Edge.Create ( 4,  9));
         T.Append (Tasks.Explore_Edge.Create ( 5, 10)); -- 10
         T.Append (Tasks.Explore_Edge.Create ( 6,  7));
         T.Append (Tasks.Explore_Edge.Create ( 6, 11));
         T.Append (Tasks.Explore_Edge.Create ( 7, 12));
         T.Append (Tasks.Explore_Edge.Create ( 8,  9));
         T.Append (Tasks.Explore_Edge.Create ( 8, 13)); -- 15
         T.Append (Tasks.Explore_Edge.Create ( 9, 10));
         T.Append (Tasks.Explore_Edge.Create ( 9, 14));
         T.Append (Tasks.Explore_Edge.Create (10, 11));
         T.Append (Tasks.Explore_Edge.Create (10, 15));
         T.Append (Tasks.Explore_Edge.Create (11, 12)); -- 20
         T.Append (Tasks.Explore_Edge.Create (11, 20));
         T.Append (Tasks.Explore_Edge.Create (12, 20));
         T.Append (Tasks.Explore_Edge.Create (12, 21));
         T.Append (Tasks.Explore_Edge.Create (13, 14));
         T.Append (Tasks.Explore_Edge.Create (13, 16)); -- 25
         T.Append (Tasks.Explore_Edge.Create (13, 24));
         T.Append (Tasks.Explore_Edge.Create (14, 15));
         T.Append (Tasks.Explore_Edge.Create (15, 19));
         T.Append (Tasks.Explore_Edge.Create (15, 20));
         T.Append (Tasks.Explore_Edge.Create (16, 17)); -- 30
         T.Append (Tasks.Explore_Edge.Create (17, 18));
         T.Append (Tasks.Explore_Edge.Create (17, 22));
         T.Append (Tasks.Explore_Edge.Create (18, 19));
         T.Append (Tasks.Explore_Edge.Create (18, 23));
         T.Append (Tasks.Explore_Edge.Create (19, 20)); -- 35
         T.Append (Tasks.Explore_Edge.Create (19, 25));
         T.Append (Tasks.Explore_Edge.Create (20, 21));
         T.Append (Tasks.Explore_Edge.Create (24, 27));
         T.Append (Tasks.Explore_Edge.Create (20, 26));
         T.Append (Tasks.Explore_Edge.Create (21, 27)); -- 40
         T.Append (Tasks.Explore_Edge.Create (24, 25));
         T.Append (Tasks.Explore_Edge.Create (25, 26));
         T.Append (Tasks.Explore_Edge.Create (26, 27));

         --  Create edges from tasks:
         for I in T.First_Index .. T.Last_Index loop
            declare
               Es : Tasks.Explore_Edge.Object
                 renames Tasks.Explore_Edge.Object (T.Element (I));
            begin
               G.Add_Undirected_Edge ((Get_From (Es),
                                       Get_To   (Es),
                                       Cost (Get_From (Es),
                                             Get_To   (Es))));
            end;
         end loop;
      end;
   end Add_Tasks;

begin
   Add_Tasks;
end Sancta.Problems.City43;
