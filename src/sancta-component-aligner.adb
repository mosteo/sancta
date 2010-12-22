with Sancta.Component.Factory,
     Sancta.Component.Types;
with Sancta.Types.Operations;

with Agpl.Conversions; use Agpl.Conversions;
with Agpl; use Agpl;

with Ada.Unchecked_Deallocation;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Aligner is

   --   use Datastore.Smart_Range_Scan_Access;

   pragma Broken ("Robot_Pose should be Robot_External_Pose??");

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Guides := Sancta.Types.Angle'Value
        (This.Option (Option_Guides, "1.570796"));
      This.Tolerance := Sancta.Types.Angle'Value
        (This.Option (Option_Tolerance, "0.17"));
      This.Out_Of_Range := Sancta.Types.Real'Value
        (This.Option (Option_Out_Of_Range, "31.0"));

      This.Avg := new Avg_Angles.Object
        (Size => Positive'Value
           (Xml.Get_Attribute (Config, "samples", "10")));

      --  Add ourselves as listeners:
      This.Subscribe (Requires_In_Pose);
      This.Subscribe (Requires_In_Scan);

      --  Store first value
      if This.Exists (Requires_In_Scan) then
         This.Output (Provides_Out_Scan, This.Input (Requires_In_Scan));
      end if;

      This.Output (Provides_Out_Pose, Types.Pose'(Pose => Sancta.Types.Origin));

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      use Sancta.Types;
   begin
      if Key = Requires_In_Pose then
         declare
            Updated_Pose : Sancta.Types.Pose := Types.Pose (Value).Pose;
         begin
            Updated_Pose.A := Updated_Pose.A + This.Delta_Angle;
            --  Store updated pose:
            This.Output (Provides_Out_Pose, Types.Pose'(Pose =>Updated_Pose));
         end;
      elsif Key = Requires_In_Scan then
         declare
            Scan      : Types.Range_Scan := Types.Range_Scan (Value);
            Old_Delta : constant Sancta.Types.Angle := This.Delta_Angle;
            New_Delta :          Sancta.Types.Angle;
            Success   :          Boolean     := False;
         begin
            begin
               declare
                  Angle_From_Scan : Sancta.Types.Angle;
               begin
                  Angle_From_Scan := Operations.Angle_From_Scan
                    (Scan.Scan.Ref.Ranges,
                     This.Out_Of_Range) +
                    Scan.Sensor_Pose_On_Robot.A;
                  declare
                     Corrected_Pose : Sancta.Types.Pose;
                  begin
                     Corrected_Pose  := Operations.Corrected_Pose
                       (Scan.Robot_External_Pose,
                        Angle_From_Scan,
                        Tolerance => This.Tolerance,
                        Guides    => This.Guides);

                     New_Delta := Corrected_Pose.A - Scan.Robot_External_Pose.A;
                  end;
               end;
               Success := True;
            exception
               when Operations.No_Angle =>
                  New_Delta := Old_Delta;
                  This.Avg.Clear;
                  --  No new correction
                  Log ("Aligner: Failed to extract orientation from scan",
                       Informative, Section => Log_Section);
               when Operations.No_Correction =>
                  New_Delta := Old_Delta;
                  This.Avg.Clear;
                  Log ("Aligner: Failed to determine compatible orientation",
                       Informative, Section => Log_Section);
            end;

            if Success and then abs (New_Delta - Old_Delta) > This.Tolerance then
               --  Not compatible: reset averager.
               Log ("Aligner: jump in angle seen!",
                    Informative, Section => Log_Section);
               This.Avg.Clear;
            end if;

            if Success then
               This.Avg.Push (New_Delta);
               This.Delta_Angle := This.Avg.Average;
            end if;

            if This.Delta_Angle /= 0.0 then
               Scan.Robot_External_Pose.A :=
                 Scan.Robot_External_Pose.A + This.Delta_Angle;
               Log ("Aligner: Corrected angle with delta " &
                    To_String (Float (This.Delta_Angle), 5),
                    Informative, Section => Detail_Section);
            end if;

            --  Store updated laser:
            This.Output (Provides_Out_Scan, Scan);

            --  Store updated pose:
            This.Output (Provides_Out_Pose,
                         Types.Pose'(Pose => Scan.Robot_Pose));
         end;
      else
         Log ("Aligner: unexpected key", Warning);
      end if;
   end Key_Stored;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Object) is
   begin
      This.Delta_Angle := 0.0;
      This.Avg.Clear;
   end Reset;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
      procedure Free is new Ada.Unchecked_Deallocation (Avg_Angles.Object'Class,
                                                        Avg_Angles.Object_Access);
   begin
      Free (This.Avg);
   end Stop;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Aligner;
