with Sancta.Network.Groups;

package body Sancta.Gui.Robot_Network_Update is

   ------------
   -- Signal --
   ------------

   procedure Signal (This : in out Object;
                     Name : in     Robot_Data.Network_Update_Kinds;
                     Data : in     Robot_Data.Network_Update)
   is
      pragma Unreferenced (Name);
   begin
      This.Link.Multicast (Network.Groups.Gui_Channel, Data);
   end Signal;

end Sancta.Gui.Robot_Network_Update;
