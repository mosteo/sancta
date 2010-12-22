with Sancta.Tasks.Goto_Pose;

package Sancta.Tasks.Complex_Goto_Pose is

   pragma Preelaborate;

   type Object is abstract new Goto_Pose.Object with null record;

   function To_Goto_Pose (This : Object) return Tasks.Goto_Pose.Object
   is abstract;
   --  Give the immediate goal for this complex navigation task.

end Sancta.Tasks.Complex_Goto_Pose;
