with Sancta.Convert;
with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Types.Transformations;

with Agpl; use Agpl;

with Interfaces.C;
with Interfaces.C.Strings;
use  Interfaces;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Novatel is

   use Types.Transformations; use Real_Transf;
   use type C.Int;

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
        (This.Option (Option_Base_Pose, "0.0 0.0 0.0"));

      Log ("Using " & Debug2.To_String (This.Base_Pose) & " as origin",
           Debug, Log_Section);

      declare
         function C_Novatel_Setup (Port : in C.Strings.chars_ptr) return C.int;
         pragma Import (C, C_Novatel_Setup, "C_Novatel_Setup");
      begin
         if C_Novatel_Setup
           (C.Strings.New_String -- Leak
              (This.Option (Option_Port, "/dev/ttyUSB0"))) /= 0
         then
            Log ("Novatel setup failed!", Error);
         else
            Log ("Novatel initialization succeeded", Informative);
         end if;
      end;

      return Component.Object_Access (This);
   end Create;

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
         Ctypes.Pose'(Pose =>
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
      use Types;

      procedure C_Novatel_Main (Result  : out C.Int;
                                X, Y    : out C.Double;
                                Elapsed : out Interfaces.Unsigned_32);
      pragma Import (C, C_Novatel_Main);
      pragma Import_Valued_Procedure (Internal => C_Novatel_Main,
                                      External => "C_Novatel_Main");

      Elapsed : Interfaces.Unsigned_32;
      X, Y    : C.Double;
      Result  : C.Int;
   begin
      Next := Clock + 0.01;

      C_Novatel_Main (Result, X, Y, Elapsed);

      declare
         New_Gps_Pose : constant Types.Pose :=
                          (Types.Real (X), Types.Real (Y), 0.0) - This.Base_Pose;
         New_Seconds  : constant Duration := Duration (Elapsed);
      begin
         if New_Gps_Pose.X /= This.Gps_Pose.X or else
           New_Gps_Pose.Y /= This.Gps_Pose.Y
--           New_Seconds /= This.Gps_Seconds
         then
            This.Gps_Seconds      := New_Seconds;
            This.Gps_Pose         := New_Gps_Pose;
            if This.exists (Requires_Odom_Pose) then
               This.Drift_Start_Pose :=
                 Ctypes.Pose (This.Input (Requires_Odom_Pose)).Pose;
            else
               This.Drift_Start_Pose := Types.Origin;
            end if;
            This.Odom_Pose        := This.Drift_Start_Pose;
            This.Gps_Pose.A       := This.Odom_Pose.A;

            --         Log ("Gps pose is " & To_String (This.Gps_Pose), Always);

            --  Update the values
            This.Output (Provides_Gps_Pose,
                         Ctypes.Pose'(Pose => This.Gps_Pose));

            This.Output (Provides_Mix_Pose,
                        Ctypes.Pose'(Pose =>
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

end Sancta.Component.Novatel;
