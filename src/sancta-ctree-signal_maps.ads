with Sancta.Map;
with Sancta.Map.Qtree;

package Sancta.Ctree.Signal_Maps is

   type Observation is new Sancta.Map.Observation with private;

   overriding
   function Is_Traversable (This : Observation) return Boolean;

   subtype Map is Sancta.Map.Qtree.Object;
   --  Not a new type, but operations for signals in this kind of map.

private

   type Observation is new Sancta.Map.Observation with record
      Samples : Sancta.Ctree.Signal_Lists.List;
   end record;

end Sancta.Ctree.Signal_Maps;
