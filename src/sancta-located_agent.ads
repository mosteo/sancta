

with Sancta.Agent;
with Sancta.Types;

--  An agent that knows its pose

package Sancta.Located_Agent is

   pragma Preelaborate;

   type Object is abstract new Sancta.Agent.Object with private;

   type Object_Access is access all Object'Class;

   function Get_Pose (This : in Object) return Types.Pose is abstract;
   function Get_Velo (This : in Object) return Types.Pose is abstract;

   procedure Set_Pose (This : in out Object; Pose : in Types.Pose) is null;
   procedure Set_Velo (This : in out Object; Velo : in Types.Pose) is null;
      --  Update the believed pose of the underlying agent.
      --  If the agent has no means to know its absolute pose (just using odom)
      --  it simply should use this as current pose.
      --  If instead he has some "absolute" source (gps, map + localization),
      --  some "incremental" pose should be used as intermediate, capisce?

private

   type Object is abstract new Sancta.Agent.Object with null record;

end Sancta.Located_Agent;
