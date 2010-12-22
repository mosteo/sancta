with Sancta.Types;

with Sancta;
with Sancta.Agent.Containers;
with Sancta.Assignment;
with Agpl.Generic_Indefinite_File_Store;
pragma Elaborate_All (Agpl.Generic_Indefinite_File_Store);
with Agpl.Graphs.Bellman_Ford; pragma Elaborate_All (Agpl.Graphs.Bellman_Ford);
with Sancta.Tasks.Containers;
with Sancta.Tasks.Primitive;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Sancta.Tasks.Grid_Goal is

   --  For experimentation in grid worlds, we use these tasks.

   --   pragma Preelaborate;

   Log_Section : constant String := "sancta.tasks.grid_goal";

   Cost_Adjust : constant Sancta.Costs := 1.0;
   --  Adjuster for costs related with this task

   package Grid_Graphs is new Agpl.Graphs.Bellman_Ford (Types.Pose);
   --  Poses will be integers, in reality, giving the matrix coordinates of
   --  the cell.

   package Cost_Store is new
     Agpl.Generic_Indefinite_File_Store (Grid_Graphs.Cost_Matrix);

   Graph : Grid_Graphs.Graph;
   --  The graph used globally by this task.
   --  This is an ugly hack used for simulation.
   --  Initialize this graph with whatever you need it to contain.

   Mission : Sancta.Tasks.Containers.Vectors.Vector;
   --  See following function.

   Agents  : Sancta.Agent.Containers.Vectors.Vector;
   --  A collection of the found agents found while loading map.
   --  Of type Sancta.Agent_Proxy.Object

   procedure Parse_Ascii (File           : in String;
                          Add_Wall_Goals : in Boolean := True);
   --  This will interpret an ASCII file as a map and load it in the two prev.
   --  vars: Graph, which contains the world, and Mission, which contains the
   --  goal tasks.
   --  If add wall goals, every empty cell adjacent to wall will be a goal.
   --  Will load costs from 'some.costs' file if found

   --  The map uses: * as obstacle, space as free way, x as goal, r as robot
   --  Example:
   --  ******
   --  *x   *
   --  * r  *
   --  ******
   --  The first line must the longest one, the rest can be shorter.
   --  Map must be wider than higher (I know, dummy-ty du)

   procedure Create_From_Poses (Poses         : in Types.Pose_Array;
                                Link_Distance : in Float := 5.0);

   type Object is new Sancta.Tasks.Primitive.Object with private;
   --  A grid position.

   function Create (Cell : in Grid_Graphs.Vertex_Index)
                    return        Object;

   Noisify : Boolean := False;
   --  If Noisify, a small amount of noise is added to the costs

   function Get_Cost (From,
                      To   : in Grid_Graphs.Vertex_Index) return Sancta.Costs;
   --  Will automatically compute the cost matrix once the first query is performed.
   --  After that point, altering the Graph will produce undefined behavior!!

   function Get_Pose (This : in Object) return Types.Pose;

   function Get_Pose (V : in Grid_Graphs.Vertex_Index) return Types.Pose;

   function Get_Cell (This : in Object) return Grid_Graphs.Vertex_Index;

   function Get_Cell (This : in Types.Pose) return Grid_Graphs.Vertex_Index;
   --  Get closest cell to some pose.

   function To_String (This : Object) return String;

   function To_Dot return Ustring;
   --  Generate a Neato graph

   function To_Dot (Ass : in Sancta.Assignment.Object) return Ustring;
   --  Generate a Neato assignment graph

   procedure Clear;
   --  Reset the global data

private

   pragma Inline (Get_Cost, Get_Pose, Get_Cell);

   type Object is new Sancta.Tasks.Primitive.Object with record
      Cell : Grid_Graphs.Vertex_Index;
   end record;

end Sancta.Tasks.Grid_Goal;
