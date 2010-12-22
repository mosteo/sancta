with Sancta.Types.Player;

package body Sancta.Player.Laser is

   -----------------
   -- Get_Reading --
   -----------------

   function Get_Reading
     (Iface : Standard.Player.Laser.Object)
      return Types.Smart_Full_Scan_Access.Object
   is
      use Player,
          Player.Laser,
          Sancta.Types,
          Sancta.Types.Player;

      Scan  : constant Types.Full_Scan_Access :=
                new Types.Full_Scan (Iface.Get_Scan_Count);
   begin
      for I in Scan.Ranges'Range loop
         declare
            Raw : constant Standard.Player.Laser.Scan := Iface.Get_Scan (I);
         begin
            Scan.Ranges (I) :=
              (A  =>   Types.Angle (Raw.Bearing),
               D  => + Raw.Rang);
            Scan.Poses (I) := (+Raw.X, +Raw.Y, 0.0);
         end;
      end loop;

      return Types.Smart_Full_Scan_Access.Bind (Scan);
   end Get_Reading;

   function Get_Reading (Iface : Standard.Player.Laser.Object)
                         return  Types.Range_Scan
   is
      use Player,
          Player.Laser,
          Sancta.Types,
          Sancta.Types.Player;

      Scan : Types.Range_Scan (1 .. Iface.Get_Scan_Count);
   begin
      for I in Scan'Range loop
         declare
            Sample : constant Standard.Player.Laser.Scan := Iface.Get_Scan (I);
         begin
            Scan (I) := (A => Sancta.Types.Angle (Sample.Bearing),
                         D => Sancta.Types.Real  (Sample.Rang));
         end;
      end loop;

      return Scan;
   end Get_Reading;

end Sancta.Player.Laser;
