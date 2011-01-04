with Sancta.Ctree.Assigner;

package body Sancta.Ctree.Strategies.Propagate is

   -------------
   -- Perform --
   -------------

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class)
   is
      pragma Unreferenced (This, Costs, Tasks);
   begin
      Ass := Perform (Ass, Links);
   end Perform;

   -------------
   -- Perform --
   -------------

   function Perform
     (Ass   :        Sancta.Assignment.Object;
      Links :        Connectivity_Matrix.Object'Class)
      return         Sancta.Assignment.Object
   is
      New_Ass : Sancta.Assignment.Object := Ass;
   begin
      Assigner.Copy_To_Linked (New_Ass, Ass.Get_Non_Idle_Agents, Links);
      return New_Ass;
   end Perform;

end Sancta.Ctree.Strategies.Propagate;
