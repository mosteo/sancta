with Player.Laser,
     Sancta.Types;

package Sancta.Player.Laser is

   function Get_Reading (Iface : Standard.Player.Laser.Object)
                         return  Types.Smart_Full_Scan_Access.Object;

   function Get_Reading (Iface : Standard.Player.Laser.Object)
                         return  Types.Range_Scan;

end Sancta.Player.Laser;
