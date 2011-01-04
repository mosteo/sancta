with Sancta.Ctree.Path_Trees,
     Sancta.Ctree.Tree_Navigator,
     Sancta.Containers,
     Sancta.Map;

use Sancta,
    Sancta.Containers;

with -- Sancta.Ctree.Obstacles,
     Sancta.Ctree.Robot,
     Sancta.Assignment,
     Sancta.Located_Agent,
     Sancta.Map.Bitmap,
     Sancta.Map.Smart,
     Sancta.Tasks,
     Sancta.Tasks.Containers,
     Sancta.Types;

use Sancta.Tasks;

package Sancta.Ctree.Utils is

   Log_Section : constant String := "Sancta.Ctree.utils";
   Det_Section : constant String := "Sancta.Ctree.utils.detail";

   type Task_Orderer is access
     function
       (Tree  : Tree_Navigator.Object'Class;
        Tasks : Tc.Lists.List) return Tc.Lists.List;

   type Orderers is (Preorder, Depth_Closest_First);

   function Preorder
     (Tree  : Tree_Navigator.Object'Class;
      Tasks : Tc.Lists.List) return Tc.Lists.List;

   function Depth_Closest_First
     (Tree  : Tree_Navigator.Object'Class;
      Tasks : Tc.Lists.List) return Tc.Lists.List;

   Available_Orderers : constant array (Orderers) of Task_Orderer :=
                          (Preorder'Access,
                           Depth_Closest_First'Access);

   function Rel_Pos (Ini, Fin : Map.Location'Class)
                     return Map.Relative_Position'Class;

   procedure Build_Tree
     (Tree  : out Path_Trees.Tree;
      Nav   :     Tree_Navigator.Object'Class;
      Tasks :     Tc.Lists.List);

   procedure Build_Costs (Node : Path_Trees.Cursor);

   function Tree_Cost (Tree : Path_Trees.Tree) return Sancta.Costs;

   type Node_Data is new Path_Trees.Node_Data with record
      Loc             : Map.Location_Handle.Object;
      Tasks           : Tc.Lists.List;     -- Tasks at a node/leaf
      Cost_From_Root  : Costs := Infinite; -- Cost going down
      Min_Branch_Cost : Costs := Infinite; -- Cheapest task in this branch

      --  These up are statically computed.
      --  These down are for simulation of costs.

      Teams_Below     : Natural := 0;
      --  One team can't go up a node that has two teams below him, would cause
      --  a deadlock with a waiting relay.
   end record;

   package TC renames Sancta.Tasks.Containers;

   --  pragma Preelaborate;

   --  These say how many goals have been reached

   procedure Check_Reached (Ass   : in out Sancta.Assignment.Object;
                            Dist  :        Types.Real);
   --  Sends beeps to Yarp remote robots

   procedure Remove_Reached (Ass   : in out Sancta.Assignment.Object;
                             Tasks : in out Sancta.Tasks.Containers.Lists.List;
                             Dist  :        Types.Real;
                             Done  :    out Natural);

   procedure Remove_Reached (Agent : in out Located_Agent.Object'Class;
                             Tasks : in out Sancta.Tasks.Containers.Lists.List;
                             Dist  :        Types.Real;
                             Done  :    out Natural);

--     function Create_Goals (Options : Config.Object;
--                            Obsts   : Obstacles.Arrays.C_Array)
--                            return    Tc.Lists.List;
--
--     function Create_Obstacles (Options : Config.Object)
--                                return    Obstacles.Arrays.C_Array;
--
--     function Estimate_Bots_Needed (Goal    : Tasks.Positioned.Object'Class;
--                                    From    : Types.Pose;
--                                    Options : Config.Object) return Positive;
--     --  Says how many robots are apparently needed to reach a goal
--     --  from From, given the K in Options
--
--     function Tasks_In_Range (Tasks : Tc.Lists.List;
--                              Stage : Positive;
--                              Opts  : Config.Object) return Tc.Lists.List;
--     --  Return tasks pertaining to ring Stage

   function Apply_To_Simple_Goal_Transform
     (Ass  : Sancta.Assignment.Object)
      return Sancta.Assignment.Object;
   --  Transforms any Complex_Goto_Pose into a basic one...

   function Apply_To_Complex_Goal_Transform
     (Ass  : Sancta.Assignment.Object;
      M    : Map.Smart.Object)
      return Sancta.Assignment.Object;
   --  Transforms simple Goto_Pose into a navigation within gridmap.

   function Avoid (M  : Sancta.Map.Bitmap.Object;
                   Cs : Float; -- Cell size
                   A  : Robot.Object) return Sancta.Types.Pose;

--     function Find_Suitable_Path (Map  : Sancta.Map.Bitmap.Smart.Object;
--                                  Bot  : Robot.Object;
--                                  Goal : Types.Pose)
--                                  return Sancta.Tasks.Goto_Pose_Bitmap_Wavefront.Object;
   --  Return, of all paths, one with minimum distancing between endpoints

--     function Find_Nearer_Path (Map  : Sancta.Map.Bitmap.Smart.Object;
--                                Bot  : Robot.Object;
--                                Goal : Types.Pose;
--                                Near : Types.Pose)
--                                return Sancta.Tasks.Goto_Pose_Bitmap_Wavefront.Object;
   --  Return, of two paths around an obstacle, the one that goes closer to Near

   type Nav_Dirs is (Backwards, Forward);

   type Navdir_Array is array (Positive range <>) of Nav_Dirs;

--     procedure Wait_For_U_Turns (Team : in out Assignment.Object;
--                                 Dirs :        Navdir_Array);
   --  Ad_Hoc for chains: if some has U-Turns, stop rest and converge U-turners
   --  Dirs *MUST* only index bots in Team (i.e. exclude base!)

end Sancta.Ctree.Utils;
