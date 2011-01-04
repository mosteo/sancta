with Ada.Containers.Ordered_Maps,
     Sancta.Containers,
     Sancta.Types;

package Sancta.Ctree.Tree_Navigator.Bitmap is

   type Object is new Tree_Navigator.Object with private;

   Log_Section : constant String := "Sancta.Ctree.tree_navigator.bitmap";

   overriding
   function Branch (This : Object;
                    Goal : Tasks.Object'Class)
                    return Map.Path;

   overriding
   function Get_Tasks (This : Object) return Tc.Lists.List;

   not overriding
   procedure Set_Tasks (This : in out Object; Jobs : Tc.Lists.List);

   not overriding
   procedure Set_Branch (This           : in out Object;
                         Goal           :        Tasks.Task_Id;
                         Path_From_Base :        Map.Path);

   not overriding
   procedure Remove_Loops (This : in out Object);

   type Creators is (Shortest_Paths,
                     Closest_Branch,
                     Cheapest_Branch,
                     Tsp,
                     Oca_A_Oca);

   type Creator_Function is access
   function
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;

   type Creator_Function_Array is array (Creators) of Creator_Function;

   function Create_With_Shortest_Paths
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;
   --  Jobs are used as ORDERED

   function Create_With_Oca_A_Oca
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;
   --  Jobs are used as ORDERED
   --  From each goal to next using best path

   function Create_With_TSP_Plan
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;
   --  Here, jobs are considered UNORDERED

   function Create_With_Closest_Branch
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;
   --  Jobs order is respected.
   --  Uses closest location in previous branch

   function Create_With_Cheapest_Branch
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class;
   --  Uses location with less backtrack + new cost

   function Available_Creators (C : Creators) return Creator_Function;
   --  Needed because of a bug that prevents using an array.

   not overriding
   procedure Set_Branch_Bypass
     (This           : in out Object;
      Goal           :        Tasks.Task_Id;
      Path_From_Base :        Map.Path);
   --  Doesn't remove loops, for internal use only

private

   package Id_Path_Maps is new Ada.Containers.Ordered_Maps
     (Tasks.Task_Id, Map.Path, Tasks."<", Map.Paths."=");

   type Object is new Tree_Navigator.Object with record
      Branches : Id_Path_Maps.Map;
      Jobs     : Tc.Lists.List;
   end record;

end Sancta.Ctree.Tree_Navigator.Bitmap;
