--  Costs from all locations to some goal

--  WARNING: it relies in ordered adition of neighbors in order not to explore
--   the full map. So some kind of triangle inequality must hold for neighbors.

generic
   with function Neighbors (M : Map.Object'Class;
                            L : Location'Class)    return Location_Vectors.Vector;
   --  Enumerate neighbors of a location.
   with function Get_Cost  (M : Map.Object'Class;
                            O : Observation'Class) return Costs;
   --  Give the cost of traversing this location.
   --  Must return infinite for non-traversable locations (if they're returned
   --   by Neighbors.)
function Sancta.Map.Create_Wavefront
  (To    : Location'Class;
   Map   : Sancta.Map.Object'Class;
   From  : Location_Handle.Object := Location_Handle.Null_Object)
   return  Loc_Cost_Maps.Map;
--  If From is given, the wavefront is built only until From is included.
