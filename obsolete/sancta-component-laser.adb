with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Datastore;
with Sancta.Component.Factory;
with Sancta.Component.Player_Safe;

with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Calendar; use Ada.Calendar;
--  with Ada.Text_Io; use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Laser is

   Laser_Skip : constant := 0;

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.As_Loc_Pose      := new String'(This.Key (Requires_Loc_Pose));
      This.As_Scan          := new String'(This.Key (Provides_Scan));
      This.As_Scan_Id       := new String'(This.Key (Provides_Scan_Id));
      This.As_Pose_On_Robot := new String'(This.Key (Provides_Pose_On_Robot));

      Player_Safe.Object.Connect_Laser_0;

      Datastore.Set_Pose (This.As_Pose_On_Robot.all,
                          Player_Safe.Object.Get_Laser_Pose);

      Log ("Laser pose reported is " &
           To_String (Player_Safe.Object.Get_Laser_Pose),
           Informative);

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Time) is
      use Types;
      New_Scan_Id : constant Integer :=
                      Player_Safe.Object.Get_Laser_Scan_Id;
   begin
      Next := Clock + 0.01;
      if New_Scan_Id - This.Scan_Id > Laser_Skip then
         This.Scan_Id := New_Scan_Id;
         --  Update the values
         Datastore.Object.Set (This.As_Scan_Id.all,
                               Datastore.Int'(Value => New_Scan_Id));
         begin
            Datastore.Object.Set (This.As_Scan.all,
              Datastore.Posed_Range_Scan'
                (Laser_Pose =>
                   Player_Safe.Object.Get_Laser_Pose,
                 Robot_Pose => Datastore.Get_Pose
                   (This.As_Loc_Pose.all),
                 Scan       => Datastore.Smart_Range_Scan_Access.Bind
                   (new Types.Range_Scan'
                      (Player_Safe.Object.Get_Laser_Scan))));
         exception
            when Protected_Datastore.Data_Not_Present =>
               Log ("Run: Missing data", Warning, Log_Section);
         end;
      end if;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Laser;
