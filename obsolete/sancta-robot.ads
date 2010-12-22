with Sancta.Gui;
with Sancta.Gui.Robot_Data.Handle;
with Sancta.Gui.Robot_Data.Messages;
with Sancta.Gui.Robot_Data.Messages2;
with Sancta.Robot_Hard;
with Sancta.Types;

with Sancta;
with Sancta.Plan;
with Sancta.Tasks;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Primitive;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;
with Agpl; use Agpl;

package Sancta.Robot is

   --  A rustic thread safety has been bolted afterwards via mutexes urgh.

   Log_Section    : constant String := "sancta.robot";
   Detail_Section : constant String := "sancta.robot.detail";

--   pragma Elaborate_Body;

   --  Strings that must be provided by some component for the robot to work.
   Key_Is_Connected     : constant String := "is_connected";
   Key_Current_Pose     : constant String := "current_pose";
   Key_Odom_Pose        : constant String := "odom_pose";
   Key_Current_Velocity : constant String := "current_velocity";
   Key_Laser_Scan       : constant String := "laser_scan";

   --  Strings that the robot provides that should be handled by some component
   Key_Goal_Action      : constant String := "goal_action";
   Key_Emergency_Stop   : constant String := "emergency_stop";

   --  The required odom pose must be the one the control platform is using to drive
   --  the robot. This way we can transform the current pose (which probably is inproved
   --  via gps, mbicp or whatever) to that reference in order to command goals.

   Connection_With_Robot_Lost : exception;

   Pi : Types.Angle renames Types.Angle_Pi;
   use type Types.Angle;

   type Object is new Robot_Hard.Object with private;
   --  This is a mess to avoid a gnat bug.

   not overriding
   procedure Add_Listener (This : in out Object;
                           X    : in     Gui.Robot_Data.Messages.Object'Class);
   --  Add a listener object to be notified when some change occurs in the robot.

   not overriding
   procedure Add_Listener2 (This : in out Object;
                            X    : in     Gui.Robot_Data.Messages2.Object'Class);
   --  Add a listener object to be notified when some change occurs in the robot.

   not overriding
   function Create (Name : in String) return Object;
   --  Initialization

   overriding
   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean);

   overriding
   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Sancta.Plan.Object);

   overriding
   function Get_Cost (This     : in Object;
                      From, To : in Sancta.Tasks.Object'Class)
                      return Sancta.Costs;

   not overriding
   function Get_Last_Pose (This : in Object) return Types.Pose;
   --  The pose of the last Positioned task in the list, current pose
   --  otherwise.

   overriding
   function Get_Pose (This : in Object) return Types.Pose;
   --  The pose according to internal localization.

   overriding
   procedure Set_Pose (This : in out Object; Pose : in Types.Pose);
   --  Set a new internal pose.

   overriding
   procedure Goto_Pose (This : in out Object; Pose : in Types.Pose);
   --  Must be called to take into account adjustments made via Set_Pose

   overriding
   function Get_Size (This : in Object) return Types.Pose;

   overriding
   function Get_Vel (This : in Object) return Types.Pose;

   overriding
   procedure Set_Vel (This    : in out Object;
                      X, Y, A : in     Types.Real);

   overriding
   function Get_Top_Angular_Speed (This : in Object) return Types.Real;

   overriding
   function Get_Top_Linear_Speed (This : in Object) return Types.Real;

   overriding
   procedure Add_Task
     (This : in out Object; The_Task : in Sancta.Tasks.Object'Class);

   overriding
   procedure Clear_Tasks (This : in out Object);
   --  Remove all assigned tasks.

   overriding
   function Get_Task_Count (This : in Object) return Natural;

   overriding
   function Get_Tasks (This : in Object)
                       return    Sancta.Tasks.Containers.Lists.List;

   overriding
   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Sancta.Tasks.Containers.Lists.List);

   overriding
   procedure Remove_Task (This : in out Object; Id : in Sancta.Tasks.Task_Id);
   --  Remove this task from this agent TO DO list.

   overriding
   function Get_Closest_Obstacle
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Reading;

   overriding
   function Get_Laser_Scan
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Scan;

   overriding
   procedure Emergency_Stop (This : in out Object);
   --  Should stop the robot and reject any future command.

   overriding
   function Is_Connected (This : in Object) return Boolean;

private

   type States is (Stop, Wander_Forward, Wander_Rotate);

   type Object is new Robot_Hard.Object with record
      Status    : States := Stop;

      Executing   : Boolean := False; -- Used to know when to send a Stop command.

      Gui_Data     : Gui.Robot_Data.Handle.Object;
      Gui_Manager  : Gui.Robot_Data.Messages.Manager;
      Gui_Manager2 : Gui.Robot_Data.Messages2.Manager;
      Gui_Command  : Ustring;
      Gui_Pose     : Types.Pose;
      Gui_Tasks    : Sancta.Tasks.Containers.Lists.List;

      Emergency_Stopped : Boolean := False;
   end record;

   procedure Register_Executers (This : in out Object);
   --  Register the known task executers

end Sancta.Robot;
