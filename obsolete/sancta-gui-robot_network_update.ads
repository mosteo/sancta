with Sancta.Gui.Robot_Data.Messages2;
with Sancta.Network;
with Sancta.Network.Layer;

package Sancta.Gui.Robot_Network_Update is

   pragma Elaborate_Body;

   type Object (Link : access Network.Layer.Object'Class) is
     new Robot_Data.Messages2.Object with private;

   procedure Signal (This : in out Object;
                     Name : in     Robot_Data.Network_Update_Kinds;
                     Data : in     Robot_Data.Network_Update);
   --  To receive robot changes notifications.

private

   type Object (Link : access Network.Layer.Object'Class) is
     new Robot_Data.Messages2.Object with null record;

end Sancta.Gui.Robot_Network_Update;
