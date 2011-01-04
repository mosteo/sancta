with Sancta.Assigner.Mtsp_Concorde,
     Sancta.Agent.Utils;

package body Sancta.Ctree.Single_Tspopt is

   ----------------
   -- Build_Plan --
   ----------------

   function Build_Plan
     (Bot    : Robot.Object;
      Jobs   : Tc.Lists.List;
      Costs  : Cost_Matrix.Object'Class;
      Init   : Boolean := True)
      return Tc.Lists.List
   is
      use Agent.Utils;
   begin
      if Init then
         declare
            New_Costs : Cost_Matrix.Object'Class := Costs;
         begin
            New_Costs.Create_Only_Start (+Bot, Jobs);
            return
              Assigner.Mtsp_Concorde.Assign
                (+Bot, Jobs, New_Costs).Get_All_Tasks;
         end;
      else
         return Assigner.Mtsp_Concorde.Assign (+Bot, Jobs, Costs).Get_All_Tasks;
      end if;
   end Build_Plan;

   ----------------
   -- Build_Plan --
   ----------------

   function Build_Plan
     (From   : Types.Pose;
      Jobs   : Tc.Lists.List;
      Costs  : Cost_Matrix.Object'Class)
      return Tc.Lists.List
   is
      Bot : Robot.Object;
   begin
      Bot.Set_Pose (From);
      Bot.Set_Name ("single_tspopt");
      return Build_Plan (Bot, Jobs, Costs, Init => True);
   end Build_Plan;

end Sancta.Ctree.Single_Tspopt;
