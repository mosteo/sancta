--  Implementation using regular UDP.

with Ada.Streams; use Ada.Streams;

package Sancta.Network.Packets is

   pragma Preelaborate;

   type Packet (<>) is private;

   function Create_Packet
     (This     : Message'Class;
      Sender   : Node_Id;
      Receiver : Address) return Packet;

   function To_Packet
     (This : Stream_Element_Array) return Packet;

   function To_Raw
     (This : Packet) return Stream_Element_Array;

   function Metadata (This : Packet) return Message_Metadata;

   function Data (This : Packet) return Stream_Element_Array;

private

   type Packet (Kind      : Address_Kinds;
                Data_Size : Stream_Element_Offset) is
      record
         Metadata : Message_Metadata (Kind);
         Data     : Stream_Element_Array (1 .. Data_Size);
      end record;

end Sancta.Network.Packets;
