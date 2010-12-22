with Sancta.Convert;
with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Component.Types;
with Sancta.Types.Operations;
with Sancta.Types.Transformations;

with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

------------------------------
-- Sancta.Component.Gps_Sim --
------------------------------

package body Sancta.Component.Gps_Sim is

   use Sancta.Types.Transformations; use Real_Transf;

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      --  Add ourselves as listeners:
      This.Subscribe (Requires_Odom_Pose);

      This.Base_Pose := Convert.To_Pose
        (This.option (Option_Base_Pose, "0.0 0.0 0.0"));

      This.Start.Reset;

      Log ("Using " & Debug2.To_String (This.Base_Pose) & " as origin",
           Debug);

      return Component.Object_Access (This);
   end Create;

   ------------------------
   -- Get_Simulated_Pose --
   ------------------------

   procedure Get_Simulated_Pose (This : in out Object;
                                 Pose :    out Sancta.Types.Pose)
   is
      Elapsed : constant Sancta.Types.Real :=
                  Sancta.Types.Real (Integer (This.Start.Elapsed) / 2 * 2);
      --  Round to nearest two second step
      Dist    : constant Sancta.Types.Real :=
                  Sancta.Types.Operations.Distance (This.Ini_Pose, This.Fin_Pose);
      Dx      : constant Sancta.Types.Real :=
                  (This.Fin_Pose.X - This.Ini_Pose.X) / Dist;
      Dy      : constant Sancta.Types.Real :=
                  (This.Fin_Pose.Y - This.Ini_Pose.Y) / Dist;
   begin
      Pose.X := This.Ini_Pose.X + Dx * 0.3 * Elapsed;
      Pose.Y := This.Ini_Pose.Y + Dy * 0.3 * Elapsed;
      Pose.A := 0.0;
      Log ("Simulated pose is " & To_String (Pose), Always);
   end Get_Simulated_Pose;

   -------------------
   -- On_Key_Stored --
   -------------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      use Types;
   begin
      pragma Assert (Key = Requires_Odom_Pose);
      This.Odom_Pose := Datastore.Pose (Value).Pose;

      --  Update the mixed pose:
      This.Output
        (Provides_Mix_Pose,
         Types.Pose'(Pose =>
                     +Compose (+This.Gps_Pose,
                       Decompose (+This.Drift_Start_Pose,
                         +This.Odom_Pose))));
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      use Sancta.Types;
      Aux : Sancta.Types.Pose;
   begin
      Next := Clock + 0.01;
      Get_Simulated_Pose (This, Aux);
      declare
         New_Gps_Pose : constant Sancta.Types.Pose :=
                          Aux - This.Base_Pose;
         New_Seconds  : constant Duration := 0.0;
      begin
         if New_Gps_Pose.X /= This.Gps_Pose.X or else
           New_Gps_Pose.Y /= This.Gps_Pose.Y
         then
            This.Gps_Seconds      := New_Seconds;
            This.Gps_Pose         := New_Gps_Pose;
            if This.Exists (Requires_Odom_Pose) then
               This.Drift_Start_Pose :=
                 Types.Pose (This.Input (Requires_Odom_Pose)).Pose;
            else
               This.Drift_Start_Pose := Sancta.Types.Origin;
            end if;
            This.Odom_Pose        := This.Drift_Start_Pose;
            This.Gps_Pose.A       := This.Odom_Pose.A;

            --  Update the values
            This.Output (provides_Gps_Pose,
                         Types.Pose'(Pose => This.Gps_Pose));

            This.Output (Provides_Mix_Pose,
                         Types.Pose'(Pose =>
                             +Compose (+This.Gps_Pose,
                                       Decompose (+This.Drift_Start_Pose,
                                                  +This.Odom_Pose))));
         end if;
      end;
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Gps_Sim;
