with Agpl.Xml,
     Sancta.Component.Player_Client,
     Sancta.Component.Player_Iface,
     Sancta.Types;

package Sancta.Component.Player_Laser is

   pragma Elaborate_Body;

   Component_Name         : aliased constant String := "player_laser";

   Log_Section            : constant String := "sancta.component.player_laser";

   Requires_Client     : Internal_Key renames Player_Iface.Requires_Client;
   Requires_Robot_Pose : constant Internal_Key := "robot_pose";

   Provides_Scan   : constant Internal_Key := "scan";
   --  of type Component.Types.Range_Scan

   Provides_Network_Scan : constant Internal_Key := "netscan";
   --  S.C.Types.Posed_Scan
   --  Ready for network redirection and drawing

   Option_Index : Option_Attr renames Player_Iface.Option_Index;
   --  Index of this interface, defaults to 0.

   Option_Skip  : constant Option_Attr := "skip";
   --  Readings to skip

   type Object (<>) is new Player_Iface.Object with private;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   function Create_Interface (This : Object) return Player_Client.Iface_Access;

   type Object is new Player_Iface.Object with record
      Pose_On_Robot : Types.Local_Pose;
      Min_Angle,
      Max_Angle     : Types.Angle;

      Prev_Scan_Id  : Integer := 0;

      Skip          : Natural := 0;
   end record;

end Sancta.Component.Player_Laser;
