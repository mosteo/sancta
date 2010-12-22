package body Sancta.Cost_Utils.Generator_Positioned is

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost
     (This : Object;
      Ini  : Sancta.Tasks.Object'Class;
      Fin  : Sancta.Tasks.Object'Class)
      return Sancta.Costs
   is
      pragma Unreferenced (This, Ini, Fin);
   begin
      return Sancta.Infinite;
   end Get_Cost;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose
     (This : in out Object;
      Pose :        Types.Pose)
   is
   begin
      This.Pose := Pose;
   end Set_Pose;

end Sancta.Cost_Utils.Generator_Positioned;
