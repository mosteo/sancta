with Sancta.Ctree.Connectivity_Matrix;

with Agpl.Chronos;
with Agpl.Containers.String_Float_Maps;
with Agpl.Counter.Multi;
with Sancta.Assignment;
with Sancta.Tasks.Containers;

package Sancta.Ctree.Stats is

   Log_Section : constant String := "nerus.stats";

   use Agpl;
   package Tc renames Sancta.Tasks.Containers;

   --  pragma Preelaborate;

   type Metrics is (Min_Max_Time,
                    Min_Ave_Time,
                    Min_Max_Odom,
                    Min_Sum_Odom,
                    Deallocations,
                    Topo_Changes);

   type Metrics_Array is array (Metrics) of Float;

   type Object is tagged limited private;

   procedure Print (This : Object);

   procedure Init (This  : in out Object;
                   Links :        Connectivity_Matrix.Object'class);

   procedure Mark_Task_Completed (This : in out Object);
   --  Used to compute AVG times
   --  Must be called by the user each time a task is completed!

   procedure Update (This          : in out Object;
                     Ass           :        Sancta.Assignment.Object;
                     Links         :        Connectivity_Matrix.Object'Class;
                     Pending_Tasks :        Tc.Lists.List);

   function Completed_Tasks (This : Object) return Natural;

   function Current_Values (This : Object) return Metrics_Array;

private

   type Object is tagged limited record
      Cron     : Chronos.Object;
      Values   : Metrics_Array := (others => 0.0);

      Bot_Odom : Containers.String_Float_Maps.Map;

      Prev_Ass   : Sancta.Assignment.Object;
      Prev_Links : Connectivity_Matrix.Object;

      Completed_Tasks : Natural := 0;

      Clusters : Counter.Multi.Object;
      Iters    : Natural := 0;
   end record;

   procedure Compute_Odometry (This : in out Object;
                               Ass  :        Sancta.Assignment.Object);
   --  Displacement of the robot between iterations

end Sancta.Ctree.Stats;
