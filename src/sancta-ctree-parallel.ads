with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Vectors;
with Sancta.Ctree.Path_Trees;
with Sancta.Ctree.Tree_Navigator;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Criteria;
with Sancta.Map;
with Sancta.Tasks;
--  with Sancta.Tasks.Handle;

package Sancta.Ctree.Parallel is

   Log_Section : constant String := "Sancta.Ctree.parallel";

   Do_Report : constant Boolean := False;
   Detailed  : constant Boolean := False;

   Do_Force  : constant Boolean := False;

   --  Be aware that there's another Ctree.Utils.Node_Data.
   --  Don't confuse them.

   package Id_Cost_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Sancta.Tasks.Task_Id, Sancta.Costs, Sancta.Tasks."<", Sancta."=");

   type Node_Data is new Path_Trees.Node_Data with private;

   type Node_Data_Array is array (Positive range <>) of Node_Data;

   type Boolean_Array is array (Positive range <>) of Boolean;

   not overriding
   function Cursor (This : Node_Data) return Path_Trees.Cursor;

   not overriding
   function Contains (This : Node_Data;
                      Job  : Sancta.Tasks.Object'Class) return Boolean;

   not overriding
   function Min_Bots_Needed (This  : Node_Data;
                             Limit : Network_Range;
                             Costs : Id_Cost_Maps.Map) return Natural;
   --  Robots for being able to at least complete some tasktask

   not overriding
   function Max_Bots_Needed (This  : Node_Data;
                             Limit : Network_Range;
                             Costs : Id_Cost_Maps.Map) return Natural;
   --  Robots needed in the worst case

   not overriding
   function Concurrent_Bots_Needed (This  : Node_Data;
                                    Limit : Network_Range;
                                    Costs : Id_Cost_Maps.Map) return Natural;
   --  Robots needed to reach all tasks in parallel

   not overriding
   function Reachable_Targets (This : Node_Data;
                               Bots : Natural;
                               Limit : Network_Range;
                               Costs : Id_Cost_Maps.Map) return Tc.Lists.List;

   not overriding
   function Tasks_Here_And_Below (This : Node_Data) return Tc.Lists.List;

   procedure Build_Tree
     (Tree  : out Path_Trees.Tree;
      Nav   :     Tree_Navigator.Object'Class;
      Tasks :     Tc.Lists.List);
   --  Builds a tree whose nodes are of the above Node_Data class

   type Teams is record
      Valid     : Boolean := False;
      Next_Node : Path_Trees.Cursor;
      Bots      : Positive;
      Id        : Natural; -- Unique id
   end record;

   type Team_Array is array (Positive range <>) of Teams;

   package Team_Vectors is new Ada.Containers.Vectors (Positive, Teams);

   type Split_Context is record
      Depth : Sancta.Costs;
      Limit : Network_Range;
      Bots  : Positive;
   end record;

   procedure Evaluate (Splitter  :     access
                         function (Conf  : Split_Context;
                                   Nodes : Node_Data_Array;
                                   Nav   : Tree_Navigator.Object'Class;
                                   Costs : Id_Cost_Maps.Map)
                                   return  Team_Array;
                       Map       :     Sancta.Map.Object'Class;
                       Navigator :     Tree_Navigator.Object'Class;
                       Tree      :     Path_Trees.Tree;
                       --  Nodes of Parallel.Node_Data
                       Bots      :     Positive;
                       Limit     :     Network_Range;
                       Criterion :     Sancta.Criteria.Enum_Criteria;
                       Stop      :     Sancta.Costs; -- Threshold for aborting
                       Cost      : out Sancta.Costs);
   --  The abort thresholds are useful when searching for the optimum.
   --  The exception below will be raised in such case:
   Cost_Exceeded : exception;

   function Split_Single_Min_Max
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array;
   --  No parallelization at all, should behave as min-max-depth-first

   function Split_Single_Preorder
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array;
   --  No parallelization at all, should behave as single preorder

   type Amount_Policies is (Sequential_Needed, Concurrent_Needed);

   type Ordered_Split_Policies is
     (Some_Task_Is_Reachable,
      --  Some task in the branch is doable.
      --  Forces extra backtracks

      Farthest_Task_Is_Reachable,
      --  Once down this branch, it will be completed.

      All_Tasks_Are_Reachable,
      --  As previous, but we delay splitting by requesting
      --  as many robots as would be needed for total concurrency.

      Farthest_Tasks_Is_Reachable_And_Go_For_Spares
      --  Obviously, there's no point in having two incomplete branches.
      --  Thus, we chose first one branch to be completely done.
      --  The remaining robots, if any, can advance work in any other
      --  branch. We take care not to send more robots than in the complete
      --  branch, since otherwise the 2nd branch would exceed time of 1st one.
     );

   Ordered_Useful : constant array (Positive range <>) of Ordered_Split_Policies :=
                      (Farthest_Task_Is_Reachable,
                       All_Tasks_Are_Reachable,
                       Farthest_Tasks_Is_Reachable_And_Go_For_Spares);

   generic
      Split_Policy  : Ordered_Split_Policies;
      Order         : Tc.Lists.List;
   function Split_Ordered
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array;
   --  Go down as many branches as possible if we have robots for the worst case

   type Heuristic_Split_Policies is (Heu_Farthest_First,
                                     Heu_Closest_First);

   generic
      Split_Policy  : Heuristic_Split_Policies;
      Amount_Policy : Amount_Policies;
   function Split_Heuristic
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array;

   type Stochastic_Split_Policies is (Full_Branch_First,
                                      Any_Branch_First);

   generic
      Split_Policy : Stochastic_Split_Policies;
   function Split_Stochastic
     (Conf  : Split_Context;
      Nodes : Node_Data_Array;
      Nav   : Tree_Navigator.Object'Class;
      Costs : Id_Cost_Maps.Map) return Team_Array;

private

   procedure Fix_Tree (T : Path_Trees.Tree);

   function Bots_Needed (Depth : Sancta.Costs;
                         Limit : Network_Range) return Positive;

   function Relays_Needed (Depth : Sancta.Costs;
                           Limit : Network_Range) return Natural;

   type Node_Data is new Path_Trees.Node_Data with record
      Pos             : Path_Trees.Cursor;
      Loc             : Sancta.Map.Location_Handle.Object;

      Blocked         : Boolean := False;
      --  Indicates if there are robots down this path,
      --  precluding another team to go there too.

      Tasks_Here      : Tc.Lists.List;     -- Tasks at a node/leaf
      Tasks_Below     : Tc.Lists.List;     -- Pending tasks below this node.

      Cost_From_Root  : Sancta.Costs := Sancta.Infinite;
      --  Cost from root to here

      Lazy_Concurrent : Natural := 0;
      --  Profiler determined this to be a speed up
   end record;

   type Timed_Teams is record
      Time_Back   : Sancta.Costs; -- Time at which this team returned to the split point
      Mobile_Bots : Positive;
      Team        : Teams;
   end record;

   function "<" (L, R : Timed_Teams) return Boolean;

   package Timed_Team_Sets is new
     Ada.Containers.Ordered_Multisets (Timed_Teams);

end Sancta.Ctree.Parallel;
