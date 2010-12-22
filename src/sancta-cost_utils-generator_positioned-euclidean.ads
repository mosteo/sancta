package Sancta.Cost_Utils.Generator_Positioned.Euclidean is

   type Object is new Generator_Positioned.Object with null record;

   overriding
   function Create (At_Pose : Types.Pose) return Object;

   overriding
   function Get_Cost (This : Object;
                      Ini  : Sancta.Tasks.Object'Class;
                      Fin  : Sancta.Tasks.Object'Class)
                      return Sancta.Costs;

end Sancta.Cost_Utils.Generator_Positioned.Euclidean;
