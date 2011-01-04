with Sancta.Ctree.Tree_Navigator;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Map;

package Sancta.Ctree.Single is

   Log_Section : constant String := "Sancta.Ctree.single";

   procedure Evaluate (Map       :     Sancta.Map.Object'Class;
                       Navigator :     Tree_Navigator.Object'Class;
                       Order     :     Tc.Lists.List;
                       Bots      :     Positive;
                       Limit     :     Network_Range;
                       Minsum    : out Sancta.Costs;
                       Minmax    : out Sancta.Costs;
                       Minave    : out Sancta.Costs);
   --  Simulate execution in single-task-at-a-time mode.
   --  We presume no replannings needed (exception raised otherwise).

end Sancta.Ctree.Single;
