with Sancta.Types;

with Sancta.Tasks.Primitive;

package Sancta.Tasks.Wander is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Primitive.Object with record
      Safe_Front : Types.Real := 1.1; -- Meters.
      Safe_Side  : Types.Real := 0.5;
      Speed      : Types.Real := 0.5; -- m/s.
      Rot_Speed  : Types.Real := 0.5; -- rad/s.
   end record;

end Sancta.Tasks.Wander;
