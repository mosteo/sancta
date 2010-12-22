with Sancta.Config;
with Sancta.Convert;
with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Component.Mbicp;
with Sancta.Component.Player_Safe;
with Sancta.Types.Transformations;

with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Gps is

   use Ada.Calendar;
   use Types.Transformations; use Real_Transf;

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.As_Odom_Pose := new String'(This.Key (Requires_Odom_Pose));
      This.As_Gps_Pose  := new String'(This.Key (Provides_Gps_Pose));
      This.As_Mix_Pose  := new String'(This.Key (Provides_Mix_Pose));

      Player_Safe.Object.Connect_Gps_0;

      --  Add ourselves as listeners:
      This.Subscribe (This.As_Odom_Pose.all);

      This.Base_Pose := Convert.To_Pose
        (Xml.Get_Attribute (Config, "base_pose", "0.0 0.0 0.0"));

      This.Reset_Mbicp := Boolean'Value
        (Xml.Get_Attribute (Config, "reset_mbicp", "true"));

      Log ("Using " & Debug2.To_String (This.Base_Pose) & " as origin",
           Debug);

      --  First dummy values:
      Datastore.Set_Pose (This.As_Gps_Pose.all,
                          Types.Origin);
      Datastore.Set_Pose (This.As_Mix_Pose.all,
                          Types.Origin);

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Agpl.Protected_Datastore.Object_Key;
                         Value : in     Agpl.Protected_Datastore.Object_Data'Class)
   is
      pragma Unreferenced (Key);
      use Types;
   begin
      This.Odom_Pose := Datastore.Pose (Value).Pose;

      --  Update the mixed pose:
      Datastore.Set_Pose (This.As_Mix_Pose.all,
                          +Compose (+This.Gps_Pose,
                                    Decompose (+This.Drift_Start_Pose,
                                               +This.Odom_Pose)));
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Time) is
      use Types;
      New_Gps_Pose : constant Types.Pose :=
                       Player_Safe.Object.Get_Gps_Pose - This.Base_Pose;
      New_Seconds  : constant Duration :=
                       Player_Safe.Object.Get_Gps_Seconds;
   begin
      Next := Clock + 0.01;
      if New_Gps_Pose.X /= This.Gps_Pose.X or else
         New_Gps_Pose.Y /= This.Gps_Pose.Y
--         New_Seconds /= This.Gps_Seconds
      then
         This.Gps_Seconds      := New_Seconds;
         This.Gps_Pose         := New_Gps_Pose;
         if Datastore.Object.Contains (This.As_Odom_Pose.all) then
            This.Drift_Start_Pose := Datastore.Get_Pose (This.As_Odom_Pose.all);
         else
            This.Drift_Start_Pose := Types.Origin;
         end if;
         This.Odom_Pose        := This.Drift_Start_Pose;
         This.Gps_Pose.A       := This.Odom_Pose.A;

--         Log ("Gps pose is " & To_String (This.Gps_Pose), Always);

         --  Reset MbICP just in case it is after us in the chain...
         if This.Reset_Mbicp then
            if Config.Get_Plugin (Component.Mbicp.Plugin_Name) /= null then
               Component.Mbicp.Object
                 (Config.Get_Plugin (Component.Mbicp.Plugin_Name).all).Reset;
            end if;
         end if;

         --  Update the values
         Datastore.Set_Pose (This.As_Gps_Pose.all,
                             This.Gps_Pose);

         Datastore.Set_Pose (This.As_Mix_Pose.all,
                             +Compose (+This.Gps_Pose,
                                       Decompose (+This.Drift_Start_Pose,
                                                  +This.Odom_Pose)));
      end if;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Gps;
