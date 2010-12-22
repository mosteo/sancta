--  Implementation using RT-WMP (Danilo Tardioli's)

with Agpl.Xml;
with Sancta.Network.Layer.Root;
with Sancta.Network.Qualities;

private with Ada.Containers.Ordered_Maps;

package Sancta.Network.Layer.Rtwmp is

   --  Specific address format for nodes is
   --  I, where I is a number between 0 .. (N - 1), where N is number of nodes.
   --  However, nodes are autoconfigured, so no need to know about this.

   Log_Section : constant String := "sancta.network.layer.rtwmp";

   type Object is limited new Root.Object with private;
   type Object_Access is access all Object'Class;

   type Rtwmp_Address is new Natural;
   type Signal_Quality is new Float range 0.0 .. 100.0;

   type Quality_Matrix is array (Rtwmp_Address range <>,
                                 Rtwmp_Address range <>) of Signal_Quality;

   function Value (Addr : Protocol_Specific_Address) return Rtwmp_Address
                   renames Rtwmp_Address'Value;

   function Image (Addr : Rtwmp_Address) return Protocol_Specific_Address;

   overriding
   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node);
   pragma Precondition (This.Id /= No_Node);
   --  Remember to call Set_Id previously.

   not overriding
   function Get_Quality (This : Object) return Quality_Matrix;
   --  Get raw, last reading of quality

   use Ada.Streams;

   overriding
   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array);

   overriding
   procedure Shutdown (This : in out Object);

   function Max_Data_Size return Stream_Element_Offset;

   --  Utilities
   subtype Rtwmp_Q_Map is Qualities.Map (Directed_Source   => True,
                                         Directed_Query    => False,
                                         Missing_As_Broken => True);

private

   overriding
   function Receive (This : Object) return Stream_Element_Array;

   package Id_Addr_Maps is new
     Ada.Containers.Ordered_Maps (Node_Id, Rtwmp_Address);

   type Object is limited new Root.Object with null record;

end Sancta.Network.Layer.Rtwmp;
