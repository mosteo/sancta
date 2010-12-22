--  Splitter/rebuilder for underlying network layers that don't support
--  large packets.

with Ada.Streams; use Ada.Streams;
with Sancta.Network.Layer.Root;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Agpl.Chronos;
private with Agpl.Sequence;

generic
   type Link_Type is new Root.Object with private;
   Mtu : Stream_Element_Count;
   --  MTU is the maximum size before splitting
package Sancta.Network.Layer.Splitter is

   Log_Section : constant String := "sancta.network.layer.splitter";

   type Object is new Link_Type with private;

   type Object_Access is access all Object'Class;

   overriding
   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array);

private

   use Agpl;

   type Sequence_Number is mod 2 ** 32;

   package Sequences is new Agpl.Sequence (Sequence_Number);

   type Header_Type is record
      Seq    : Sequence_Number;
      Index  : Positive;
      Length : Stream_Element_Count;
   end record;
   --  pragma Pack (Header_Type);
   --  Raises a warning and then Unchecked_Conversion fails... it seems the
   --  packing is not being honored at all places?

   Header_Octets : constant Stream_Element_Count :=
                     Header_Type'Size / Stream_Element'Size;

   Max_Data_Size : constant Stream_Element_Count := Mtu - Header_Octets - 1;

   type Boolean_Array is array (Positive range <>) of Boolean;

   type Holder_Type (Parts  : Positive;
                     Length : Stream_Element_Count) is record
      Data    : Stream_Element_Array (1 .. Length);
      Missing : Boolean_Array (1 .. Parts) := (others => True);

      Since   : Chronos.Object;
   end record;

   package Seq_Holder_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Sequence_Number, Holder_Type);

   type Object is new Link_Type with record
      Self        : access Object := Object'Unchecked_Access;
      Purge_Timer : Chronos.Object;
      Partials    : Seq_Holder_Maps.Map;
   end record;

   overriding
   function Receive (This : Object) return Stream_Element_Array;

   not overriding
   procedure Purge (This : in out Object);

   not overriding
   procedure Assemble (This     : in out Object;
                       Raw      :        Stream_Element_Array;
                       Complete :    out Seq_Holder_Maps.Cursor);
   --  Assemble expects the raw packet, including full byte flag + header + data

end Sancta.Network.Layer.Splitter;
