package body Sancta.Ctree.Context is

   --------------
   -- Base_Bot --
   --------------

   function Base_Bot (This : Object) return Robot.Object is
      Bot : Robot.Object;
   begin
      Bot.Set_Name (This.Base);
      Bot.Set_Pose (This.Base_Pose);
      return Bot;
   end Base_Bot;

end Sancta.Ctree.Context;
