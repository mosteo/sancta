with Sancta.Component.Player_Client;
with Sancta.Component.Player_Graphics2d;
with Sancta.Component.Player_Laser;
with Sancta.Component.Player_Localize;
with Sancta.Component.Player_Position2d;

package body Sancta.Component.Player_Include is

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Player_Client.Register;
      Player_Position2d.Register;
      Player_Laser.Register;
      Player_Localize.Register;
      Player_Graphics2d.Register;
   end Register;

end Sancta.Component.Player_Include;
