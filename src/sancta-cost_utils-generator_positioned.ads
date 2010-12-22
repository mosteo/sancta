package Sancta.Cost_Utils.Generator_Positioned is

   type Object is abstract new Cost_Generator with private;

   not overriding
   function Create (At_Pose : Types.Pose) return Object is abstract;

   overriding
   function Get_Cost (This : Object;
                      Ini  : Sancta.Tasks.Object'Class;
                      Fin  : Sancta.Tasks.Object'Class)
                      return Sancta.Costs;
   --  Returns always infinite.

   overriding
   procedure Set_Pose (This : in out Object;
                       Pose :        Types.Pose);

private

   type Object is abstract new Cost_Generator with record
      Pose : Types.Pose;
   end record;

end Sancta.Cost_Utils.Generator_Positioned;
