with Sancta.Types;

with Sancta.Plan_Node;
with Sancta.Method;
with Sancta.Tasks;

with Ada.Numerics;

package Sancta.Methods.Panorama_Expansion is

   pragma Preelaborate;

   use type Types.Real;

   type Object is new Sancta.Method.Object with record
      FOV : Types.Real := Ada.Numerics.Pi / 4.0;
   end record;
   --  This configures the Field of View of the available cameras.

   subtype Result is Sancta.Plan_Node.Node_Access;

   function Apply (This : in Object; That : in Sancta.Tasks.Object'Class)
                   return Result;
   --  Applies to Panorama_At_Coords tasks.
   --  Generates Snapshot_At_Pose tasks.
   --  Will make eight snapshots at cardinals points and bisectrices.

end Sancta.Methods.Panorama_Expansion;
