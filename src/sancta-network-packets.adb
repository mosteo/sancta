with Agpl.Streams.Circular_Unbounded;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Network.Packets is

   -------------------
   -- Create_Packet --
   -------------------

   function Create_Packet
     (This     : Message'Class;
      Sender   : Node_Id;
      Receiver : Address)
      return Packet
   is
--        L1  : Logger (new String'("Pkt:Pre-Raw"), Always);
      Raw : constant Stream_Element_Array := To_Raw (This);
--        L2  : Logger (new String'("Pkt:Pst-Raw"), Always);
   begin
--      Log ("RAW:" & Raw'Length'Img, Always);
      return
        (Kind      => Network.Kind (Receiver),
         Data_Size => Raw'Length,
         Metadata  => (Kind      => Network.Kind (Receiver),
                       Sender    => Sender,
                       Receiver  => Receiver),
         Data      => Raw);
   end Create_Packet;

   ---------------
   -- To_Packet --
   ---------------

   function To_Packet
     (This : Stream_Element_Array)
      return Packet
   is
      use Agpl.Streams.Circular_Unbounded;
      Stream : aliased Stream_Type;
   begin
      Create (Stream);
      Stream.Write (This);

      declare
         P : constant Packet := Packet'Input (Stream'Access);
      begin
--           Log ("IN :" & This'First'Img, Always);
--           Log ("IN :" & This'Last'Img, Always);
--           Log ("IN :" & p.Data_Size'Img, Always);
         return P;
      end;
   end To_Packet;

   ------------
   -- To_Raw --
   ------------

   function To_Raw
     (This : Packet)
      return Stream_Element_Array
   is
      use Agpl.Streams.Circular_Unbounded;
      Stream : aliased Stream_Type;
   begin
      Create (Stream);
      Packet'Output (Stream'Access, This);
      declare
         Buffer : Stream_Element_Array (1 .. Stream.Available_Read);
         Last   : Stream_Element_Offset;
      begin
         Stream.Read (Buffer, Last);
         pragma Assert (Last = Buffer'Last);

         return Buffer;
      end;
   end To_Raw;

   --------------
   -- Metadata --
   --------------

   function Metadata (This : Packet) return Message_Metadata is
   begin
      return This.Metadata;
   end Metadata;

   ----------
   -- Data --
   ----------

   function Data (This : Packet) return Stream_Element_Array is
   begin
      return This.Data;
   end Data;

end Sancta.Network.Packets;
