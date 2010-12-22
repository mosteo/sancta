with Player.Laser,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Debug2,
     Sancta.Player.Laser;
with Sancta.Types.Player;

with Agpl; use Agpl;

with Ada.Calendar; use Ada.Calendar;
--  with Ada.Text_Io; use Ada.Text_Io;

use Sancta.Debug2;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Player_Laser is

   use type Sancta.Types.Pose;

   subtype Local_Pose is  Sancta.Types.Local_Pose;
   --  subtype Global_Pose is Sancta.Types.Global_Pose;

   type Object_Access is access all Object;

   ----------------------
   -- Create_Interface --
   ----------------------

   function Create_Interface (This : Object) return Player_Client.Iface_Access
   is
      pragma Unreferenced (This);
   begin
      return new Standard.Player.Laser.Object;
   end Create_Interface;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Sancta.Types.Player;

      This : constant Object_Access :=
               new Object (Name   => Component_Name'Access,
                           Config => Config);
      Next : Ada.Calendar.Time;
   begin
      This.Client.all.Sync;

      declare
         procedure Get_Laser_Pose (Iface : Player_Client.Iface_Access) is
            Laser : Standard.Player.Laser.Object renames
              Standard.Player.Laser.Object (Iface.all);
         begin
            This.Pose_On_Robot := Local_Pose (+Laser.Get_Robot_Pose);
            Log ("Laser pose reported is " &
                 To_String (Sancta.Types.Pose (This.Pose_On_Robot)),
                 Informative);
         end Get_Laser_Pose;
      begin
         This.Execute (Get_Laser_Pose'Access);
      end;

      declare
         Foo : Standard.Player.Double;
         Baz : Boolean;
         procedure Get_Laser_Config (Iface : Player_Client.Iface_Access) is
            Laser : Standard.Player.Laser.Object renames
              Standard.Player.Laser.Object (Iface.all);
         begin
            Laser.Get_Config (Standard.Player.Double (This.Min_Angle),
                              Standard.Player.Double (This.Max_Angle),
                              Foo, Foo, Baz, Foo);
         end Get_Laser_Config;
      begin
         This.Execute (Get_Laser_Config'Access);
      end;

      if This.Exists (Option_Skip) then
         This.Skip := Natural'Value (This.Option (Option_Skip, ""));
      end if;

      This.Run (Next);

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Time) is
      use Sancta.Types;
      use Sancta.Types.Player;

      procedure Safe (Iface : Player_Client.Iface_Access) is
         Laser : Standard.Player.Laser.Object renames
           Standard.Player.Laser.Object (Iface.all);
         New_Scan_Id : constant Integer := Laser.Get_Scan_Id;
      begin
         if New_Scan_Id - This.Prev_Scan_Id > This.Skip then
            This.Prev_Scan_Id := New_Scan_Id;
            --  Update the values
            This.Output
              (Provides_Scan,
               Sancta.Component.Types.Range_Scan'
                 (Scan_Id              => New_Scan_Id,
                  Sensor_Pose_On_Robot => This.Pose_On_Robot,
                  Robot_Pose           => +Laser.Get_Robot_Pose,
                  Robot_External_Pose  =>
                    Types.Pose (This.Input (Requires_Robot_Pose)).Pose,
                  Scan                 =>
                    Sancta.Player.Laser.Get_Reading (Laser)));

            This.Output
              (Provides_Network_Scan,
               Sancta.Component.Types.Posed_Scan'
                 (Length              => Laser.Get_Scan_Count,
                  Robot_Pose          => Types.Pose (This.Input (Requires_Robot_Pose)).Pose,
                  Laser_Pose_On_Robot => Sancta.Types.Pose (This.Pose_On_Robot),
                  Samples             => Sancta.Player.Laser.Get_Reading (Laser),
                  Ini                 => This.Min_Angle,
                  Fin                 => This.Max_Angle));
         end if;
      end Safe;
   begin
      Next := Clock + 0.01;
      This.Execute (Safe'Access);
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Component_Name, Create'Access);
   end Register;
end Sancta.Component.Player_Laser;
