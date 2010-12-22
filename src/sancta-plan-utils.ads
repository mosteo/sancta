 

package Sancta.Plan.Utils is

   pragma Preelaborate;

   Log_Section : constant String := "Sancta.plan.utils";

   procedure Replace_Child (This        : in out Plan.Object;
                            Parent_Node,
                            New_Child,
                            Old_Child   : in     Subplan);
   --  Replace the Old_Child (whose parent is Parent_Node) by a new one.
   --  The Old is freed! The New is deep copied (because the entire branch
   --  is freed) but no extra memory is left dangling.

   procedure Trim_OR_Siblings (This : in out Plan.Object;
                               Job  : in     Tasks.Task_Id);
   --  Given a plan and a task id, will replace an OR branch containing the
   --  Task by the task proper.
   --  This is useful for example to merge an assignment with a plan and ob-
   --  tain a plan compatible

end Sancta.Plan.Utils;
