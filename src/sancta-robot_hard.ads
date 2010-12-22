with Sancta.Located_Agent;
with Sancta.Types;

package Sancta.Robot_Hard is

   pragma Preelaborate;

   Pi : Types.Angle renames Types.Angle_Pi;
   use type Types.Angle;

   type Object is abstract new Located_Agent.Object with null record;
   --  This is a mess to avoid a gnat bug.

   type Object_Access is access all Object'Class;

   procedure Goto_Pose (This : in out Object; Pose : in Types.Pose) is abstract;
   --  Must be called to take into account adjustments made via Set_Pose

   function Get_Size (This : in Object) return Types.Pose is abstract;

   function Get_Vel (This : in Object) return Types.Pose is abstract;

   procedure Set_Vel (This    : in out Object;
                      X, Y, A : in     Types.Real) is abstract;

   function Get_Top_Angular_Speed (This : in Object) return Types.Real
   is abstract;

   function Get_Top_Linear_Speed (This : in Object) return Types.Real
   is abstract;

   function Get_Closest_Obstacle
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Reading is abstract;

   function Get_Laser_Scan
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Scan is abstract;

   procedure Emergency_Stop (This : in out Object) is abstract;
   --  Should stop the robot and reject any future command.

   function Is_Connected (This : in Object) return Boolean is abstract;

end Sancta.Robot_Hard;
