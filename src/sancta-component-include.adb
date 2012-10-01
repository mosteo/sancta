with Sancta.Component.Agent_To_Tasks;
with Sancta.Component.Aligner;
with Sancta.Component.Auctioneer;
with Sancta.Component.Bidder_Flat;
with Sancta.Component.Bitmap,
     Sancta.Component.Bitmap_Tasks,
     Sancta.Component.Coord_Transf;
with Sancta.Component.Copier;
with Sancta.Component.Cost_Cache,
     Sancta.Component.Cost_Updater;
with Sancta.Component.Draw,
     Sancta.Component.Executor_Manual_Driving,
     Sancta.Component.Executor_Others,
     Sancta.Component.Executor_Posed;
with Sancta.Component.Generator_Pose,
     Sancta.Component.Generic_Mixer,
     Sancta.Component.Gps_Sim;
with Sancta.Component.Helper;
with Sancta.Component.Located_Agent,
     Sancta.Component.Logger_Montesano,
     Sancta.Component.Logger_Pose,
     Sancta.Component.Manual_Pose_Correction,
     Sancta.Component.Manual_Tasks,
     Sancta.Component.Map_Tasks,
     Sancta.Component.Mbicp,
     Sancta.Component.Mbicp_Filter,
     Sancta.Component.Merger;
with Sancta.Component.Netstats,
     Sancta.Component.Network,
     Sancta.Component.Novatel,
     Sancta.Component.Port_Waiter,
     Sancta.Component.Qmap,
     Sancta.Component.Random_Goto_Tasks,
     Sancta.Component.Redirect,
     Sancta.Component.Redirect_Laser;
with Sancta.Component.Redirect_Listener,
     Sancta.Component.Shared_Database,
     Sancta.Component.Spawn;
with Sancta.Component.Switch;
with Sancta.Component.Task_Completion_Checker,
     Sancta.Component.Task_Listener,
     Sancta.Component.Task_Watchdog,
     Sancta.Component.Team,
     Sancta.Component.Team_Splitter,
     Sancta.Component.Tsp,
     Sancta.Component.Watchdog,
     Sancta.Component.Worst_Cost;

package body Sancta.Component.Include is

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Agent_To_Tasks.Register;
      Aligner.Register;
      Auctioneer.Register;
      Bidder_Flat.Register;
      Bitmap.Register;
      Bitmap_Tasks.Register;
      Coord_Transf.Register;
      Copier.Register;
      Cost_Cache.Register;
      Cost_Updater.Register;
      Draw.Register;
      Executor_Posed.Register;
      Executor_Manual_Driving.Register;
      Executor_Others.Register;
      Generator_Pose.Register;
      Generic_Mixer.Register;
      Helper.Register;
      Located_Agent.Register;
      Manual_Pose_Correction.Register;
      Manual_Tasks.Register;
      Map_Tasks.Register;
      Merger.Register;
      Netstats.Register;
      Network.Register;
      Port_Waiter.Register;
      Redirect.Register;
      Redirect_Laser.Register;
      Redirect_Listener.Register;
      Spawn.Register;
      Switch.Register;
      Task_Completion_Checker.Register;
      Task_Listener.Register;
      Task_Watchdog.Register;
      Team.Register;
      Team_Splitter.Register;
      Tsp.Register;
      Qmap.Register;
      Watchdog.Register;
      Worst_Cost.Register;
   end Register;

end Sancta.Component.Include;
