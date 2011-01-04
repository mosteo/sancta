package Sancta.Ctree.Strategies.Idle_Hungarian is

   --  pragma Preelaborate;

   --  This particular strategy has been modified so, as long as a bot in the
   --  scluster has a task, the rest do not receive one.

   Log_Section : constant String := "nerus.strategies.idle_hungarian";

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
                     Links  : Connectivity_Matrix.Object'Class)
                     return   Sancta.Assignment.Object;
   --  Convenience function for static use

end Sancta.Ctree.Strategies.Idle_Hungarian;

