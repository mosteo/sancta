with Sancta.Tasks.Positioned;

with Sancta.Tasks.Starting_Pose;

package body Sancta.Cost_Utils.Generator_Positioned.Manhattan is

   use type Sancta.Tasks.Task_Id;
   use type Types.Real;

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
      P1 : Types.Pose;
      P2 : Types.Pose;
   begin
      if Fin in Sancta.Tasks.Starting_Pose.Object'Class then
         return Sancta.Infinite;
      elsif Ini in Sancta.Tasks.Starting_Pose.Object'Class then
         P1 := This.Pose;
      else
         P1 := Tasks.Positioned.Object (Ini).Pose;
      end if;

      P2 := Tasks.Positioned.Object (Fin).Pose;

      return Sancta.Costs
        ((abs (P1.X - P2.X)) + (abs (P1.Y - P2.Y)));
   end Get_Cost;

end Sancta.Cost_Utils.Generator_Positioned.Manhattan;
