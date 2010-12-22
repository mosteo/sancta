with Sancta.Component.Factory;
with Sancta.Types.Transformations;

with Agpl.Calendar.Format;
with Agpl.Conversions;
--  with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Ada.Text_Io;  use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Logger_Montesano is

   use type Types.Range_Scan;
   use Sancta.Types.Transformations; use Real_Transf;

   --------------
   -- Add_Scan --
   --------------

   procedure Add_Scan (This : in out Object;
                       Scan : in     Types.Range_Scan)
   is
      use Sancta.Types;
      R : Reading;
   begin
      if Scan.Robot_External_Pose = This.Prev_Pose then
         return;
      end if;

      This.Prev_Pose := Scan.Robot_External_Pose;

      if This.Use_Laser_Pose then
         R.Odom :=
           +Compose (+Scan.Robot_External_Pose,
                     +Sancta.Types.Pose (Scan.Sensor_Pose_On_Robot));
      else
         R.Odom := +Compose (+Types.Pose (This.Input (Requires_Pose)).Pose,
                             +Sancta.Types.Pose (Scan.Sensor_Pose_On_Robot));
      end if;

      for I in Scan.Scan.Ref.Ranges'Range loop
         R.Dist.Append (Scan.Scan.Ref.Ranges (I).D);
      end loop;

      if This.Immediate then
         if not This.First_Written then
            This.First_Written := True;
            Put_Line (This.File, Natural'Image (Natural (R.Dist.Length)));
         end if;

         Write_Reading (R, This.File);
         Flush (This.File);
      else
         This.Data.Append (R);
      end if;
   end Add_Scan;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      use Agpl.Xml;
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Filename := new String'
        (This.option
           (Option_Filename,
            "lm." & Agpl.Calendar.Format.Datestamp (Separator =>'.') &"."&
                    Agpl.Calendar.Format.Timestamp & ".log"));

      This.Immediate := Boolean'Value
        (This.Option (Option_Immediate, "false"));

      This.Use_Laser_Pose := Boolean'Value
        (This.option (Option_Use_Laser_Pose, "true"));

      This.Subscribe (Requires_Scan);

      if This.Immediate then
         Create (This.File, Out_File, This.Filename.all);
      end if;

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
      if Key = requires_Scan then
         Add_Scan (This, Types.Range_Scan (Value));
      end if;
   end Key_Stored;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      if This.Immediate then
         Close (This.File);
      else
         This.Write_Scans;
      end if;
   end Stop;

   -------------------
   -- Write_Reading --
   -------------------

   procedure Write_Reading (This : in Reading;
                            File : in out Ada.Text_Io.File_Type)
   is
      use Agpl.Conversions;
   begin
      for I in This.Dist.First_Index .. This.Dist.Last_Index loop
         Put_Line
           (File,
            To_String (Long_Long_Float (This.Dist.Element (I)), 5));
      end loop;
      Put_Line (File,
                To_String (Long_Long_Float (This.Odom.X), 5) & " " &
                To_String (Long_Long_Float (This.Odom.Y), 5) & " " &
                To_String (Long_Long_Float (This.Odom.A), 5));
   end Write_Reading;

   -----------------
   -- Write_Scans --
   -----------------

   procedure Write_Scans (This : in Object) is
      F : File_Type;
   begin
      Create (F, Out_File, This.Filename.all);
      Put_Line
        (F,
         Natural'Image (Natural (This.Data.Length)) & " " &
         Natural'Image (Natural (This.Data.First_Element.Dist.Length)));

      for R in This.Data.First_Index .. This.Data.Last_Index loop
         declare
            Readings : Reading renames This.Data.Element (R);
         begin
            Write_Reading (Readings, F);
         end;
      end loop;

      Close (F);
   end Write_Scans;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;
end Sancta.Component.Logger_Montesano;
