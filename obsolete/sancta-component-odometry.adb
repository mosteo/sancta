with Sancta.Convert; use Sancta.Convert;
with Sancta.Datastore;
with Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Component.Player_Client;

with Player;
--  with Player.Client;

with Agpl.Trace; use Agpl.Trace;

pragma Elaborate_All (Sancta.Component.Factory);

--  Player component.
--  Provides the Position2d_0 interface from player
--  Sends commands also to that interface.
--  Because of that, the goal pose should be in the same reference that the
--  player interface is using.

--  Example: if position2d:0 is VFH reading from position2d:1 (odometry),
--  we should give the goal pose in the position2d:1 reference
--  (which in this case is the same as position2d:0).

package body Sancta.Component.Odometry is

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.As_Pose     := new String'(This.Key (Provides_Pose));
      This.As_Pose2    := new String'(This.Key (Provides_Pose2));
      This.As_Velocity := new String'(This.Key (Provides_Velocity));

      Component.Player_Client.Create_And_Subscribe (This.Pos);

      declare
         Dummy : Ada.Calendar.Time;
      begin
         This.Run (Dummy); -- Insert first values
      end;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      P : constant Player.Pose := This.Pos.Get_Pose;
      V : constant Player.Pose := This.Pos.Get_Velocity;
   begin
      Next := Clock + 0.001;

      Log ("Robot pose: " & Debug2.To_String (+P), Debug, Log_Section);

      Datastore.Set_Pose (This.As_Pose.all, +P);
      Datastore.Set_Pose (This.As_Pose2.all, +P);
      Datastore.Set_Pose (This.As_Velocity.all, +V);
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Odometry;
