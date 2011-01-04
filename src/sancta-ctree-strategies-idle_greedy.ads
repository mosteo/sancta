package Sancta.Ctree.Strategies.Idle_Greedy is

   --  pragma Preelaborate;

   Log_Section : constant String := "nerus.strategies.idle_greedy";

   type Object is new Strategies.Object with null record;

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class);

   function Perform (Agents : Ac.Lists.List;
                     Tasks  : Tc.Lists.List;
                     Costs  : Sancta.Cost_Cache.Object'Class;
                     Links  : Connectivity_Matrix.Object'Class;
                     Propag : Boolean := True)
                     return   Sancta.Assignment.Object;
   --  if not Propag, several connected robots could have conflicting tasks!

end Sancta.Ctree.Strategies.Idle_Greedy;

