with Sancta.Types;

with Sancta.Tasks.Compound;

package Sancta.Tasks.Explore_Segment is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Compound.Object with private;
   --  A segment defined by two points.
   --  The sense is not stablished, planner can choose best one.

   function Create (From, To : in Types.Pose) return Object;

   function Get_From (This : in Object) return Types.Pose;

   function Get_To (This : in Object) return Types.Pose;

   function To_String (This : Object) return String;

private

   type Object is new Sancta.Tasks.Compound.Object with record
      P1, P2 : Types.Pose;
   end record;

end Sancta.Tasks.Explore_Segment;
