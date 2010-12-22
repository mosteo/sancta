with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Types.Real_Math;

with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

-----------------------------------
-- Sancta.Component.Mbicp_Filter --
-----------------------------------

package body Sancta.Component.Mbicp_Filter is

   use Types.Real_Math;

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Link_Distance := Types.Real'Value
        (This.Option (Option_Link_Distance, "2.0"));
      This.Pass_Distance := Types.Real'Value
        (This.Option (option_pass_distance, "2.0"));
      This.Scan_Angle := Types.Real'Value
        (This.Option (Option_Scan_Angle, "0.32"));
      This.Out_Of_Range := Types.Real'Value
        (This.Option (Option_Out_Of_Range, "31.0"));

      --  Add ourselves as listeners:
      This.Subscribe (Requires_Scan);

      --  Store dummy value
      This.Output
        (Provides_Filtered_Scan,
         Ctypes.Range_Scan'
           (Robot_External_Pose => Types.Null_Pose,
            Sensor_Pose_On_Robot => Types.Local_Pose (Types.Null_Pose),
            Scan                 => Types.Smart_Full_Scan_Access.Bind
              (new Types.Full_Scan (0)),
            others => <>));

      return Component.Object_Access (This);
   end Create;

   -------------------
   -- On_Key_Stored --
   -------------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      use Types;
      Scan : Types.Range_Scan := Ctypes.Range_Scan (Value).Scan.Ref.Ranges;
      Acum_Length : Types.Real := 0.0;
      Gap_Start   : Positive   := Scan'First;
   begin
      Log ("*******", Debug, Section => Log_Section);

      --  Cut out of ranges for visual pleasure
      for I in Scan'Range loop
         Scan (I).D := Types.Real'Min (Scan (I).D, This.Out_Of_Range + 1.0);
      end loop;

      --  Proper filtering
      for I in Scan'First + 1 .. Scan'Last loop
         declare
            Jump : constant Types.Real :=
                     abs (Scan (I).D - Scan (I - 1).D);
         begin
            if Jump <= This.Link_Distance and then I /= Scan'Last then
               Acum_Length :=
                 Acum_Length +
                   Sin (This.Scan_Angle) * Types.Real'Min (Scan (I).D,
                                                           Scan (I - 1).D);
            else
               if Acum_Length < This.Pass_Distance then
                  for J in Gap_Start .. I - 1 loop
                     Scan (J).D := This.Out_Of_Range;
                  end loop;
                  Log ("Acum: " & To_String (Float (Acum_Length)) &
                       "; Filtered" & Integer'Image (I - Gap_Start) & " points",
                       Debug, Section => Log_Section);
               else
                  Log ("Acum: " & To_String (Float (Acum_Length)) &
                       "; Passed" & Integer'Image (I - Gap_Start) & " points",
                       Debug, Section => Log_Section);
               end if;
               Gap_Start   := I;
               Acum_Length := 0.0;
            end if;
         end;
      end loop;

      pragma Uuuurgh ("Very unthread safe");
      pragma Incomplete ("Only ranges have been adjusted, what of the x/y?");
      Ctypes.Range_Scan (Value).Scan.Ref.all.Ranges := Scan;
      This.Output (Provides_Filtered_Scan, Value);
   end Key_Stored;

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Mbicp_Filter;
