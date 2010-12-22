

with Sancta.Cost_Utils;
with Sancta.Cost_Utils.Generator_Handle;
with Sancta.Located_Agent;
with Sancta.Types;

with Sancta;
with Sancta.Plan;
with Sancta.Tasks;
with Sancta.Tasks.Primitive;

with Ada.Calendar; use Ada.Calendar;

--  Allows to set info about a remote agent, and compute task costs.
--  But nothing else!

package Sancta.Agent_Proxy is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.agent_proxy";

   type Object is new Located_Agent.Object with private;

   type Object_Access is access all Object'Class;

   function Get_Alive (This : in Object) return Boolean;

   function Get_Last_Seen (This : in Object) return Time;

   procedure Set_Alive (This : in out Object; Alive : in Boolean := True);
   --  Informative; just to know if an agent has been seen recently.

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean);

   procedure Set_Cost_Generator
     (This : in out Object;
      Gen  :        Cost_Utils.Cost_Generator'Class);

   overriding
   function Get_Cost (This : in Object; From, To : in Sancta.Tasks.Object'Class)
                      return Sancta.Costs;
   --  Must say how takes for the robot to do To assuming his last assignment
   --  was From.
   --  This default dispatchs to Sancta.Costs.Get_Execution_Cost by default.
   --  Unless a cost generator has been set.
   --  Should return Costs'Last for undoable tasks, never raise Constraint_Error.

   function Get_Id (This : in Object) return Node_Id;

   procedure Set_Id (This : in out Object; Id : in Node_Id);

   function Get_Pose (This : in Object) return Types.Pose;
   function Get_Velo (This : in Object) return Types.Pose;

   procedure Set_Pose (This : in out Object;
                       Pose : in     Types.Pose);
   --  Sets the pose locally, but isn't propagated to the remote robot.

   procedure Set_Velo (This : in out Object;
                       Velo : in     Types.Pose);

   procedure Set_Ang_Speed (This  : in out Object;
                            Speed : in     Types.Real);

   procedure Set_Lin_Speed (This  : in out Object;
                            Speed : in     Types.Real);

   overriding
   function Finished (This : Object;
                      Job  : Tasks.Object'Class) return Boolean;

   not overriding
   procedure Print (This : Object);
   --  Debug output of Name-Location-Velocity

private

   type Object is new Located_Agent.Object with record
      Pose      : Types.Pose := Types.Null_Pose;
      Velo      : Types.Pose := Types.Null_Pose;

      Lin_Speed : Types.Real      := 0.5;
      Ang_Speed : Types.Real      := 0.5;

      Alive     : Boolean         := True;
      Last_Seen : Time            := Clock;

      Id        : Node_Id := No_Node;

      Cost_Gen  : Cost_Utils.Generator_Handle.Object;
   end record;

end Sancta.Agent_Proxy;
