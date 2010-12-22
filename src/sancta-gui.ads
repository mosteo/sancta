with Sancta.Network;

package Sancta.Gui is

   --  pragma Preelaborate;

   Drag_N_Drop_Pose : constant String := "pose/2d";
   --  This string identifies the kind of data associated with robot poses
   --  for drag'n'drop in the visor.

   Drag_N_Drop_Pose_Flag : constant := 0;

   Gui_Channel       : constant Network.Channel := Network.Value ("gui");
   Auction_Channel   : constant Network.Channel := Network.Value ("auction");
   Emergency_Channel : constant Network.Channel := Network.Value ("emergency");

end Sancta.Gui;
