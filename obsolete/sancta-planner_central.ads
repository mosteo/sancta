 

--  A centralized planner based in HTNs.
--  Customizable to use any Assigner.

with Sancta.Gui.Robot_Data;
with Sancta.Netlistener;
with Sancta.Network;

with Agpl.Chronos;
with Sancta.Agent.Containers;
with Sancta.Assignment;
with Sancta.Method;
with Sancta.Plan;
with Sancta.Tasks.Containers;
with Agpl; use Agpl;

--  Basic infrastructure sharable by centralized planners.

package Sancta.Planner_Central is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "PlannerCentral";
   Detail_Section : constant String := "PlannerCentral.detail";
   --  To selectively enable debug messages...

   Default_Aliveness : constant Duration := 5.0;
   --  Time until a silent robot is considered unusable.

   Default_Wait      : constant Duration := 1.0;
   --  Time to receive new tasks (for they come in batchs) until replanning

   type Object is abstract new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   procedure Add_Method (This   : in out Object;
                         Method : in     Sancta.Method.Object'Class);
   --  Method to be used by the planner.

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class);
   --  Add a new task to be planned

   function Get_Alive_Agents (This : in Object) return Sancta.Agent.Containers.Lists.List;
   --  Get known clients (of Agent_Proxy class)

   function Get_Planner (This : in Object) return Sancta.Plan.Object;
   --  Returns a plan with the methods registered but without tasks.

   function Get_Pending_Tasks (This : in Object) return Sancta.Tasks.Containers.Lists.List;
   --  Get tasks pending to be planned.

   procedure Init (This : in out Object);
   --  Subscribe to the GUI, Emergency and Management
   --  Call this one if you override it.

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);
   --  Do something with a received packet.
   --  Call this one if you override it.

   procedure Replan (This   : in out Object) is abstract;
   --  This is the procedure to override by descendent planners.
   --  It's called when Wait time has elapsed since the reception of a new
   --  task.
   --  Get_Alive_Agents and Get_Pending_Tasks can be useful here.
   --  Get_Pending_Tasks will return an empty list after the call to Replan.

   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Run a control step. Should be called periodically.
   --  If you extend it, you should call the parent Run to keep
   --  current functionality.

   procedure Send_Plan_To_Agents (This       : in Object;
                                  Assignment : in Sancta.Assignment.Object);
   --  Send new instructions to agents.
   --  This is not automagically called, you should do it from Replan or
   --  another place if necessary.

   procedure Set_Configuration (This      : in out Object;
                                Wait      : in     Duration := Default_Wait;
                                Aliveness : in     Duration := Default_Aliveness);
   --  Wait: time to wait for a new task before start planning.
   --  Aliveness: time without notice from a bot until declaring it dead.

private

   procedure Add_Or_Update_Agent (This : in out Object;
                                  Meta : in     Network.Message_Metadata);

   procedure Check_Lost_Bots (This : in out Object);
   --  Will mark as dead bots not giving a HELLO in the last seconds.

   procedure Update_Agent_Pose (This : in out Object;
                                M    : in     Gui.Robot_Data.Network_Update'Class;
                                Meta : in     Network.Message_Metadata);
   --  M is of Pose kind (already verified in reception).
   --  Also the robot is already stored via Add_Or_Update_Agent

   type Object is abstract new Netlistener.Object with
      record
         Wait            : Duration := Default_Wait;
         Aliveness       : Duration := Default_Aliveness;

         Last_Task_Time  : Chronos.Object;

         Agents          : Sancta.Agent.Containers.Maps.Map;
         --  The indexing key is the sender address image.

         Plan            : Sancta.Plan.Object; -- Plan with methods.

         Pending_Tasks   : Sancta.Tasks.Containers.Lists.List; -- Tasks pending of plan/assignation.
         Replan_Needed   : Boolean := False;     -- Marks if a replan is pending.
      end record;

end Sancta.Planner_Central;
