with Agpl.Trace,

     Sancta.Ctree.Components,
--     Sancta.Ctree.Component.Console,
--       Sancta.Component.Netstats,
--     Sancta.Component.Netstats2Gnetstats,
--       Sancta.Component.Network_Rtwmp,
     Sancta.Component.Player_Include,
     Sancta.Component.Pose2Robot_Pose,
     Sancta.Config,
     Sancta.Datastore,
     Sancta.Starter;

use Agpl,
    Agpl.Trace,
    Sancta;

with Gnat.Os_Lib;

with Ada.Command_Line; use Ada.Command_Line;

procedure CTree_Node is
begin
   Log (Command_Name & " starting...", Always);

--   Sancta.Ctree.Component.Console.Register;
   Sancta.Ctree.Components.Register;
--   Sancta.Component.Netstats2Gnetstats.Caster.Register;
--     Sancta.Component.Network_Rtwmp.Register;
   Sancta.Component.Player_Include.Register;
   Sancta.Component.Pose2Robot_Pose.Caster.Register;

   Sancta.Starter.Launch;

   while
     (not Sancta.Datastore.Object
        (Config.Get_Id).Contains ("finished")) or else
     (not Sancta.Datastore.Bool
        (Sancta.Datastore.Object (Config.Get_Id).Get ("finished")).Value)
   loop
      delay 0.11;
   end loop;

   Log ("Connect_Tree exiting...", Always);
   delay 2.0;
   Gnat.Os_Lib.Os_Exit (0);

exception
   when E : others =>
      Log (Command_Name & " [Main]: " & Report (E), Error);
      delay 1.0;
      Gnat.Os_Lib.Os_Exit (1);
end CTree_Node;
