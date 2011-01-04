package Sancta.Ctree.Strategies.Propagate is

   --  pragma Preelaborate;

   type Object is new Strategies.Object with null record;

   procedure Perform
     (This  : in out Object;
      Ass   : in out Sancta.Assignment.Object;
      Tasks :        Tc.Lists.List;
      Costs :        Sancta.Cost_Cache.Object'Class;
      Links :        Connectivity_Matrix.Object'Class);

   function Perform
     (Ass   :        Sancta.Assignment.Object;
      Links :        Connectivity_Matrix.Object'Class)
      return         Sancta.Assignment.Object;

end Sancta.Ctree.Strategies.Propagate;

