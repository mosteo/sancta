--  Algorithm to compute paths in a grid. NC-1 I think.
--  Requires triangle inequality respect. Only the first route found is kept.
--  Thus the neighbors function should not mislead on this.

generic
   with function Neighbors (M : Map.Object'Class;
                            L : Location'Class)    return Location_Vectors.Vector;
   --  Enumerate neighbors of a location.
   with function Get_Cost  (M : Map.Object'Class;
                            O : Observation'Class) return Costs;
   --  Give the cost of traversing this location.
   --  Must return infinite for non-traversable locations (if they're returned
   --   by Neighbors.)
function Sancta.Map.Wavefront
  (From,
   To    : Location'Class;
   Map   : Sancta.Map.Object'Class)
   return  Location_Lists.List;
   --  Returns ordered vector of locations giving the route.
