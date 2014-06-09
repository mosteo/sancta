--  By means of this package we force the compilation and inclusion of
--  the necessary object code for these task.
--  If not, the call to Sancta.Tasks.Unserialize will fail with Ada.Tags.Tag_Error

pragma Warnings (Off); -- To avoid Unreferenced claims.

--  with Sancta.Methods.Choose_Entry_Point;
--  with Sancta.Methods.Explore_Segment_Expansion;
--
with Sancta.Network.Messages;
--  with Sancta.Problems.City43;
--
--  with Sancta.Tasks.Choose_Entry_Point;
--  with Sancta.Tasks.Entry_Point;
--  with Sancta.Tasks.Explore_Directed_Segment;
--  with Sancta.Tasks.Explore_Segment;
--  with Sancta.Tasks.Explore_Directed_Edge;
--  with Sancta.Tasks.Explore_Edge;
with Sancta.Tasks.Goto_Pose;
--
with Sancta.Tasks.Still;

pragma Warnings (On);

package Sancta.Tasks.Include is

end Sancta.Tasks.Include;
