generic
   with function Neighbors (M : Map.Object'Class;
                            L : Location'Class)    return Location_Vectors.Vector;
   --  Enumerate neighbors of a location.
function Sancta.Map.Build_Wavefront_Route
  (From,
   To    : Location'Class;
   Wave  : Loc_Cost_Maps.Map;
   Map   : Sancta.Map.Object'Class)
   return  Location_Lists.List;
