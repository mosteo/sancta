with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Types.Transformations;

with Agpl.Chronos;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Mbicp;

--  with Ada.Text_Io; use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Mbicp is

   package Icp renames Standard.Mbicp;
   subtype Sm_Float is Icp.Mbicp_Float; use type Sm_Float;

   type Object_Access is access all Object;

   ----------------------
   --  Calculator Type --
   ----------------------

   Have_Previous : Boolean := False;
   task body Calculator_Type is
      use Types;
      Previous_Scan : Ctypes.Range_Scan;
      Current_Scan  : Ctypes.Range_Scan;

      Previous_Pose,
      Current_Pose,
      Pose_Delta    : Types.Pose := Types.Null_Pose;

      -------------------
      -- To_Mbicp_Scan --
      -------------------

      function To_Mbicp_Scan (X : in Ctypes.Range_Scan)
                              return Icp.Laser_Scan
      is
         Result : Icp.Laser_Scan (1 .. X.Scan.Ref.Ranges'Length);
      begin
         for I in Result'Range loop
            Result (I).Bearing  := Icp.Mbicp_Float (X.Scan.Ref.Ranges (I).A);
            Result (I).Distance := Icp.Mbicp_Float (X.Scan.Ref.Ranges (I).D);
         end loop;
         return Result;
      end To_Mbicp_Scan;

      -------------
      -- Compute --
      -------------

      procedure Compute is
         Timer   : Chronos.Object;
         R       : Icp.Pose;
         Result  : Types.Pose;
         Final   : Types.Pose;
         Outcome : Icp.Outcomes;
         use Types.Transformations;
         use Types.Transformations.Real_Transf;
         Laser_Pose : constant Types.Pose :=
                        Ctypes.Pose
                          (Parent.Input (Requires_Laser_Pose_On_Robot)).Pose;
         --  Where's the laser on the robot
      begin
         begin
            --  The delta must be given in lasercentric coordinates!
            Pose_Delta := +Decompose (Compose (+Previous_Pose,
                                               +Laser_Pose),
                                      Compose (+Current_Pose,
                                               +Laser_Pose));

            Icp.Match (To_Mbicp_Scan (Previous_Scan),
                       To_Mbicp_Scan (Current_Scan),
                       (Icp.Mbicp_Float (Pose_Delta.X),
                        Icp.Mbicp_Float (Pose_Delta.Y),
                        Icp.Mbicp_Float (Pose_Delta.A)),
                       R,
                       Outcome);

            Result := (Real (R.X), Real (R.Y), To_Angle (Real (R.A)));
            --  Since MBICP always returns a lasercentric result and the odometry
            --  is respect to the robot starting pose, we must transform the MBICP
            --  result to the odometry frame:

            Log ("Odom said " & To_String (Pose_Delta),
                 Debug, Section => Log_Section);
            Log ("MICP said " & To_String (Result),
                 Debug, Section => Log_Section);

            --  The result in odometry frame
            Final := +Compose (+Parent.Mbicp_Pose.Get,
                               Compose (Compose (+Laser_Pose,
                                                 +Result),
                                        Invert  (+Laser_Pose)));

            Log ("Elapsed: " & Timer.Elapsed'Img & "; Outcome: " & Outcome'Img,
                 Debug, Section => Log_Section);

            if Outcome in Icp.Outcomes_Ok then
               Parent.Mbicp_Pose.Set (Final);
            else
               --  Failed, we follow odometry
               Log ("Mbicp: " & Outcome'Img & "(Elapsed: " & Timer.Elapsed'Img & ")",
                    Warning, Log_Section);
               Parent.Mbicp_Pose.Set
                 (+Compose (+Parent.Mbicp_Pose.Get,
                            Decompose (+Previous_Pose,
                                       +Current_Pose)));
            end if;
         exception
            when E : Constraint_Error =>
               Log ("Component.MBICP: " & Report (E),
                    Warning, Log_Section);
               --  Log ("Odom: " & To_String (Pose_Delta), Warning);
               --  Log (" ICP: " & To_String (Result),     Warning);
            when Matrix_Singular =>
               Log ("Component.MBICP: Matrix singular (too small delta?)",
                    Warning);
         end;

         Previous_Scan := Current_Scan;
         Previous_Pose := Current_Pose;

         --  Update the generated pose and improved scan:
         Parent.Output (Provides_Improved_Pose,
                        Ctypes.Pose'(Pose => Parent.Mbicp_Pose.Get));
         Previous_Scan.Sensor_Pose_On_Robot := Types.Local_Pose (Laser_Pose);
         Previous_Scan.Robot_External_Pose  := Parent.Mbicp_Pose.Get;
         Parent.Output (Provides_Improved_Scan, Previous_Scan);
      end Compute;

   begin
      loop
         select
            accept New_Scan (Scan : in Ctypes.Range_Scan;
                             Pose : in Types.Pose)
            do
               Current_Scan := Scan;
               Current_Pose := Pose;
            end New_Scan;
         or
            terminate;
         end select;
         if Have_Previous and then Current_Pose /= Previous_Pose then
            Compute;
         elsif not Have_Previous then
            Previous_Scan := Current_Scan;
            Previous_Pose := Current_Pose;
            Have_Previous := True;
         elsif Have_Previous and Current_Pose = Previous_Pose then
            --  Passthru a laser update, the laser is not static even if pose is.
            Current_Scan.Robot_Pose := Parent.Mbicp_Pose.Get;
            Parent.Output (Provides_Improved_Scan,
                           Current_Scan);
         end if;
      end loop;
   exception
      when E : others =>
         Log ("Component.Mbicp.Calculator: " & Report (E), Error,
              Log_Section);
   end Calculator_Type;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Calculator := new Calculator_Type (This);

      --  Add ourselves as listeners:
      This.Subscribe (Requires_Odom_Pose);
      This.Subscribe (Requires_Scan);

      Icp.Set_Out_Of_Range
        (ICP.Mbicp_Float'Value
           (This.Option (option_out_of_range, "8.0")));

      --  Store dummy value
      This.Output (Provides_Improved_Scan,
        Ctypes.Range_Scan'
          (Sensor_Pose_On_Robot  => Types.Local_Pose (Types.Null_Pose),
           Robot_External_Pose   => Types.Null_Pose,
           Scan                  =>
             Types.Smart_Full_Scan_Access.Bind
               (new Types.Full_Scan (0)),
           others => <>));

      This.Output (Provides_Improved_Pose,
                   Ctypes.Pose'(Pose => Types.Origin));

      Icp.Init
        (Bw => Float'Value
           (Xml.Get_Attribute (Config, "bw", Float'Image (1.57 / 3.0))));

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
      if Key = Requires_Odom_Pose then
         --  Initialize MbICP pose if not done already:
         if not This.Mbicp_Pose_Inited then
            This.Mbicp_Pose_Inited := True;
            This.Odom_Base_Pose := Ctypes.Pose (Value).Pose;
            This.Mbicp_Pose.Set (Ctypes.Pose (Value).Pose);
            --  And update the provided odom pose:
            This.Output (Provides_Improved_Pose, Value);
         end if;

         --  Keep last known pose:
         This.Odom_Pose.Set (Ctypes.Pose (Value).Pose);

         --  Passthru Pose:
         --  Now I think this is wrong!! should be a composition as always!!
         raise Program_Error;
         pragma Ojo ("Should be checked!!!");
         This.Output (Provides_Improved_Pose,
           Ctypes.Pose'(Pose =>
                          This.Mbicp_Pose.Get +
                            Ctypes.Pose (Value).Pose -
                          This.Odom_Base_Pose));
      elsif Key = Requires_Scan and then This.Mbicp_Pose_Inited then
         This.Odom_Base_Pose :=
           Ctypes.Pose (This.Input (Requires_Odom_Pose)).Pose;
         --  Start a new range calculation if calculator ready:
         select
            This.Calculator.New_Scan (Ctypes.Range_Scan (Value),
                                      This.Odom_Pose.Get);
         else
            Log ("Component.Mbicp: Laser scan dropped [calculator busy]",
                 Warning, Log_Section);
         end select;
      end if;
   end Key_Stored;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Object) is
   begin
      Have_Previous          := False;
      This.Mbicp_Pose_Inited := False;
   end Reset;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Mbicp;
