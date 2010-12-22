with Sancta.Types;

with Sancta;
with Agpl.Graphs.Bellman_Ford;
with Sancta.Tasks.Compound;

pragma Elaborate_All (Agpl.Graphs.Bellman_Ford);

package Sancta.Tasks.Explore_Edge is

   pragma Preelaborate;

   Cost_Adjust : constant Float := 10.0;
   --  Adjuster for costs related with this task

   package Pose_Graphs is new Agpl.Graphs.Bellman_Ford (Types.Pose);

   Graph : Tasks.Explore_Edge.Pose_Graphs.Graph;
   --  The graph used globally by this task.
   --  This is an ugly hack used for simulation.
   --  Initialize this graph with whatever you need it to contain.

   type Object is new Sancta.Tasks.Compound.Object with private;
   --  A segment defined by two points.
   --  The sense is not stablished, planner can choose best one.

   function Create (From, To : in Pose_Graphs.Vertex_Index)
                    return        Object;

   function Get_Cost (From,
                      To   : in Pose_Graphs.Vertex_Index) return Sancta.Costs;
   --  Will automatically compute the cost matrix once the first query is performed.
   --  After that point, altering the Graph will produce undefined behavior!!

   function Get_Pose (V : in Pose_Graphs.Vertex_Index) return Types.Pose;

   function Get_From (This : in Object) return Pose_Graphs.Vertex_Index;

   function Get_To (This : in Object) return Pose_Graphs.Vertex_Index;

   function To_String (This : Object) return String;

private

   pragma Inline (Get_Cost, Get_Pose, Get_From, Get_To);

   type Object is new Sancta.Tasks.Compound.Object with record
      V1,
      V2 : Pose_Graphs.Vertex_Index;
   end record;

end Sancta.Tasks.Explore_Edge;
