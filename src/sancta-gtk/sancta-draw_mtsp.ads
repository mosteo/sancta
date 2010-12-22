with Sancta.Agent_Proxy;
with Sancta.Types; use Sancta.Types;

with Sancta.Agent.Containers;
with Sancta.Assignment;
with Agpl.Optimization.Concorde; use Agpl.Optimization.Concorde;
with Agpl; use Agpl;

package Sancta.Draw_Mtsp is

   --  pragma Elaborate_Body;

   --  Non-modal drawing using Agpl.Gdk.Managed.Drawing_Area

   pragma Elaborate_Body;

   procedure From_Agent (This : in Agent_Proxy.Object);
   --  Single traveler solution.

   procedure From_Assignment (This  : Sancta.Assignment.Object;
                              Title : String := "");
   --  Agents from Agent_Proxy class.
   --  Tasks from positioned class.

   procedure From_Poses (Poses : in Pose_Array;
                         Tour  : in Normal_Tour;
                         Title : in String := "");
   --  Draw the tour given the poses and the solution.
   --  The cities and the Poses indexes must match.

   procedure From_Tasks (Agents : in Sancta.Agent.Containers.Lists.List;
                         Title  : in String := "");
   --  Tasks must be of Positioned class.
   --  The agents must be of Agent_Proxy class.

end Sancta.Draw_Mtsp;
