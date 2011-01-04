with Ada.Containers.Ordered_Maps;
with Sancta;
with Sancta.Network.Qualities;

pragma Warnings (Off);
with Agpl.Trace; use Agpl.Trace;
pragma Warnings (On);


package Sancta.Ctree is

   pragma Preelaborate;

   --  Root for things related with the Connect_Tree paper

   type Robot_Id is new Positive;

   function Image (Id : Robot_Id) return String;   pragma Inline (Image);
   function Value (Id : String)   return Robot_Id; pragma Inline (Value);

   type Link_Qualities is new Float range 0.0 .. 9999.9;
   --  This is misleading. Actually, 0.0 is infinite quality.
   --  Basically is a distance.

   function To_Quality (F : Float) return Link_Qualities;

   subtype Signal_Q is Sancta.Network.Qualities.Quality;
   --  This is a proper quality, with 0.0 being no connection and 100.0 perfect.

   use type Signal_Q;

   function To_Signal_Q (F : Float) return Signal_Q;
   pragma Inline (To_Signal_Q);

   type Alloc_Strategies is
     (Greedy, Greedy_One, Greedy_Dumb, Hungarian,
      Clock, Paraclock, Freeclock,
      Minmax, Minmix, Minsum,
      Tsp, Tspold, Tspopt,
      Tree_Tspopt, -- A tspopt optimized for Tree_*, obsoletes idle strat.
      Tree_From_Base,
      Tree_Depth,
      Tree_Depth_Minave,
      Tree_Depth_Parallel,
      Taskbids,
      Outwards, -- Order by MinSum, choosing in staged steps (predicting)
      Outwards_Free, -- Choose freely in each stage, outwards (predicting)
      Outwards_Minmax,
      Outwards_Minave,
      Outwards_Minsum,
      Outwards_Minsumold,
      Binary_Minmax,
      Binary_Minave,
      Binary_Minsum,
      Classic_Minmax, -- Regular auctions, one robot-task, no propagate or hungarian
      Classic_Minave,
      Classic_Minsum,
      Complex);

   type Group_Strategies is (None,
                             Propagate_Goal,
                             Tree_And_Follow,
                             Tree_And_Relay,
                             Tree_From_Base,
                             Tree_Depth,
                             Tree_Depth_Shortcut);
   --  Strategies for a robot group to adopt:
   --  * Do nothing (except for the leader)
   --  * Use the leader task
   --  * Move to the nearest nexus node (helps in chain formation)
   --    Only one robot has goal, the closest one in the s-cluster

   subtype Navigational_Strategies is
     Group_Strategies range Tree_And_Follow .. Group_Strategies'Last;

   type Idle_Strategies is (None, Greedy, Cluster, Hungarian);

   type Spring_Strategies is (None, Keep_Weak, Keep_Weak_Without_Idle);

   type Data_Sources is (Source_Integrated, Source_Yarp);

   type Steps is (Tspopt,
                  Draw_Plan);

   type Robot_Configs is record
      Mobile : Boolean;
   end record;

   subtype Network_Range is Float range 0.0 .. Float'Last;

   package Id_Q_Maps is
     new Ada.Containers.Ordered_Maps (Sancta.Node_Id,
                                      Signal_Q,
                                      Sancta."<");
   --  This is quality from the point of view of a single robot, since only the
   --  remote id is included.

   --  For now, this global shit:
   Umbral : constant Float := 4.0;
   Leeway : constant Float := 1.25;

   Bail_Out : Boolean := False;

private

   pragma Convention (C, Robot_Id);

end Sancta.Ctree;
