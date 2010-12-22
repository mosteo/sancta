with Sancta.Component.Root,
     Sancta.Types;

package Sancta.Component.Mbicp_Filter is

   pragma Elaborate_Body;

   Log_Section : constant String := "component.mbicp_filter";

   Name : aliased constant String := "mbicp_filter";

   Requires_Scan          : constant Internal_Key := "scan";
   Provides_Filtered_Scan : constant Internal_Key := "filtered_scan";

   Option_Link_Distance : constant Option_Attr := "link_distance";
   Option_Pass_Distance : constant Option_Attr := "pass_distance";
   Option_Scan_Angle    : constant Option_Attr := "scan_angle";
   Option_Out_Of_Range  : constant Option_Attr := "out_of_range";

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   procedure Register;

private

   type Object is new Root.Object with
   record
      Link_Distance     : Types.Real;
      Pass_Distance     : Types.Real;
      Scan_Angle        : Types.Real;
      Out_Of_Range      : Types.Real;
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Mbicp_Filter;
