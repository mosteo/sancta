with Sancta.Tasks.Positioned;
with Sancta.Types.Operations; use Sancta.Types.Operations;

with Sancta.Tasks.Starting_Pose;

package body Sancta.Cost_Utils.Generator_Positioned.Euclidean is

   use type Sancta.Tasks.Task_Id;

   ------------
   -- Create --
   ------------

   function Create (At_Pose : Types.Pose) return Object is
      This : constant Object := (Pose => At_Pose);
   begin
      return This;
   end Create;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : Object;
      Ini  : Sancta.Tasks.Object'Class;
      Fin  : Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
   begin
      return Get_Execution_Cost (Ini, Fin, This.Pose, 1.0, 1.0);

      if Fin in Sancta.Tasks.Starting_Pose.Object'Class then
         return Sancta.Infinite;
      elsif Ini in Sancta.Tasks.Starting_Pose.Object'Class then
         return Sancta.Costs
           (Distance (This.Pose, Tasks.Positioned.Object (Fin).Pose));
      else
         return Sancta.Costs
           (Distance (Tasks.Positioned.Object (Ini).Pose,
                      Tasks.Positioned.Object (Fin).Pose));
      end if;
   end Get_Cost;

end Sancta.Cost_Utils.Generator_Positioned.Euclidean;
