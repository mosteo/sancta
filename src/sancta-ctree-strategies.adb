package body Sancta.Ctree.Strategies is

   -------------------
   -- Set_Base_Pose --
   -------------------

   procedure Set_Base_Pose (This : in out Object;
                            Pose :        Sancta.Types.Pose) is
   begin
      This.Base_Pose := Pose;
      end Set_Base_Pose;

   -------------------
   -- Get_Base_Pose --
   -------------------

   function Get_Base_Pose (This : Object) return Sancta.Types.Pose is
   begin
      return This.Base_Pose;
   end Get_Base_Pose;

end Sancta.Ctree.Strategies;
