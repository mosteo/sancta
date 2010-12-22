with Sancta.Component.Root,
     Sancta.Types;

with Agpl.Average_Queue;
with Agpl.Constants;
with Agpl.Xml;

package Sancta.Component.Aligner is

   pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.Component.aligner";
   Detail_Section : constant String := "sancta.Component.aligner.detail";

   Name : aliased constant String := "aligner";

   Requires_In_Scan  : constant Internal_Key := "in_scan";
   Requires_In_Pose  : constant Internal_Key := "in_pose";

   Provides_Out_Pose : constant Internal_Key := "out_pose";
   Provides_Out_Scan : constant Internal_Key := "out_scan";

   Option_Guides    : constant option_attr := "guides";
   --  Angle of the walls (radians)

   Option_Tolerance : constant Option_Attr := "tolerance";
   --  Adjust if within this angle

   Option_Out_Of_Range : constant Option_Attr := "out_of_range";
   --  Laser is out of range beyond this value

   Option_Samples : constant Option_Attr := "samples";
   --  Num. of samples to use for some average

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access;

   not overriding
   procedure Reset (This : in out Object);
   --  Will cause resetting of angle to the odometric one.

   overriding
   procedure Stop (This : in out Object);

   procedure Register;

private

   package Avg_Angles is new Agpl.Average_Queue (Types.Angle);

   type Object is new Root.Object with
   record
      Guides      : Types.Angle := Agpl.Constants.Pi_2;
      Tolerance   : Types.Angle := 0.17;
      Out_Of_Range: Types.Real  := 31.0;

      Avg          : Avg_Angles.Object_Access;
      Delta_Angle  : Types.Angle := 0.0;
      --  Delta will be the Avg, cached to avoid re-computation

      --  Convenience fields:
      As_In_Pose,
      As_Out_Pose,
      As_In_Scan,
      As_Out_Scan : access String;
      end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Aligner;
