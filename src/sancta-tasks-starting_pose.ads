 

with Sancta.Tasks.Primitive;

package Sancta.Tasks.Starting_Pose is

   pragma Preelaborate;

   type Object (<>) is new Sancta.Tasks.Primitive.Object with private;
   --  This special task is used to simplify the manipulation of task lists
   --  so there's no special case for the first task of an agent.

   --  For it to work, each Agent'Class implementation should treat this task
   --  as of cost 0 if matches its name and its the first task, infinite otherwise.
   --  Conversely, the cost from this task to any other is the cost from the agent
   --  current position to the task.

   --  As a variation, instead of reporting the cost from current pose to the task,
   --  it should report the cost from the last finished task to the next one.
   --  In this way, partially completed task lists can be evaluated using real
   --  posteriori costs plus estimated costs, and, in an ideal world, everything
   --  should match and be the same.

   --  Or, even better, if it reports the cost of all past tasks plus the
   --  estimation to the next, then no jumps in cost will occur.

   function Create (For_Agent : in String) return Object;

   function Get_Name (This : in Object) return String;

private

   type Object (Name_Len : Natural) is new Sancta.Tasks.Primitive.Object with record
      Agent_Name : String (1 .. Name_Len);
   end record;

end Sancta.Tasks.Starting_Pose;
