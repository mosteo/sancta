with Ada.Unchecked_Conversion;
with Agpl.Strings;

package body Sancta.Network.Layer.Splitter is

   --  First byte is reserved for the special case of unsplitted packets:
   --  0 means a full packet is here, 1 means a header + part follows.

   Full_Packet : constant Stream_Element := 0;
   Part_Packet : constant Stream_Element := 1;

   subtype Header_Stream is Stream_Element_Array (1 .. Header_Octets);

   function To_Stream is new Ada.Unchecked_Conversion
     (Header_Type, Header_Stream);

   function To_Header is new Ada.Unchecked_Conversion
     (Header_Stream, Header_Type);

   Sequence : Sequences.Object;

   Max_Ttl : constant Duration := 1.0;

   ----------
   -- Info --
   ----------

   function Info (Header : Header_Type) return String is
      use Agpl.Strings;
   begin
      return
        Trim (Header.Seq'Img) & "/" &
        Trim (Header.Index'Img) & "/" &
        Trim (Header.Length'Img);
   end Info;

   ----------
   -- Send --
   ----------

   procedure Send
     (This : in out Object;
      Dest : in     Node_Id;
      Data : in     Stream_Element_Array)
   is
      use Agpl.Strings;
      Parts : constant Stream_Element_Count :=
                (Data'Length - 1) / Max_Data_Size + 1;
      Seq   : Sequence_Number;
   begin
      if Data'Length < Mtu - 1 then
         Log ("Sending one-piece packet", Debug, Log_Section);
         Link_Type (This).Send (Dest, (1 => Full_Packet) & Data);
      elsif Header_Octets + 2 > Mtu then
         raise Constraint_Error
           with "Transport doesn't support enough packet size";
      else
         Sequence.Get_Next (Seq);
         for I in 1 .. Parts loop
            Log ("(Seq/Part/Tot):" &
                 Seq'Img & Rpad (Trim (I'Img), 4) & Rpad (Trim (Parts'Img), 4),
                 Debug, Log_Section);

            declare
               Header : constant Header_Type :=
                          (Seq    => Seq,
                           Index  => Positive (I),
                           Length => Data'Length);
            begin
               Link_Type (This).Send
                 (Dest,
                  (1 => Part_Packet) &
                  To_Stream (Header) &
                  Data (Data'First + (Max_Data_Size * (I - 1)) ..
                        Stream_Element_Offset'Min
                          (Data'Last,
                           Data'First + (Max_Data_Size * I) - 1)));
            end;
         end loop;
      end if;
   end Send;

   -------------
   -- Receive --
   -------------

   function Receive
     (This : Object)
      return Stream_Element_Array
   is
   begin
      loop
         declare
            Raw : constant Stream_Element_Array := Link_Type (This).Receive;
         begin
            if Raw (Raw'First) = Full_Packet then
               This.Self.Purge;
               Log ("Full packet received", Debug, Log_Section);
               return Raw (Raw'First + 1 .. Raw'Last);
            else
               Log ("Part packet received", Debug, Log_Section);
               declare
                  Complete : Seq_Holder_Maps.Cursor;
               begin
                  This.Self.Assemble (Raw, Complete);
                  if Seq_Holder_Maps.Has_Element (Complete) then
                     declare
                        Result : constant Stream_Element_Array :=
                                   Seq_Holder_Maps.Element (Complete).Data;
                     begin
                        This.Self.Partials.Delete (Complete);
                        return Result;
                     end;
                  end if;
                  This.Self.Purge;
               end;
            end if;
         end;
      end loop;
   end Receive;

   -----------
   -- Purge --
   -----------

   procedure Purge
     (This : in out Object)
   is
      use Seq_Holder_Maps;
      I : Cursor := This.Partials.First;
      J : Cursor;

      Count : Natural := 0;
   begin
      if This.Purge_Timer.Elapsed <= 1.0 then
         return;
      end if;

      while Has_Element (I) loop
         J := Next (I);

         if Element (I).Since.Elapsed > Max_Ttl then
            This.Partials.Delete (I);
            Count := Count + 1;
         end if;

         I := J;
      end loop;

      This.Purge_Timer.Reset;

      Log ("Purged:" & Count'Img, Debug, Log_Section);
   end Purge;

   --------------
   -- Assemble --
   --------------

   procedure Assemble (This     : in out Object;
                       Raw      :        Stream_Element_Array;
                       Complete :    out Seq_Holder_Maps.Cursor)
   is
      use Seq_Holder_Maps;
      Header : constant Header_Type :=
                 To_Header (Raw (Raw'First + 1 .. Raw'First + Header_Octets));

      I      : Cursor := This.Partials.Find (Header.Seq);
      Ok     : Boolean;

      -------------------
      -- Create_Holder --
      -------------------

      function Create_Holder (Header : Header_Type) return Holder_Type is
      begin
         return (Parts   => Positive ((Header.Length - 1) / Max_Data_Size + 1),
                 Length  => Header.Length,
                 Data    => <>,
                 Missing => (others => True),
                 Since   => <>);
      end Create_Holder;

      --------------
      -- Assemble --
      --------------

      procedure Assemble (Seq : Sequence_Number; Holder : in out Holder_Type) is
         pragma Unreferenced (Seq);
         Done : Boolean := True;
      begin
         if not Holder.Missing (Header.Index) then
            Log ("Discarding already known part " & Info (Header),
                 Debug, Log_Section);
            return;
         end if;

         Log ("Storing part " & Info (Header), Debug, Log_Section);

         Holder.Missing (Header.Index) := False;
         for I in Holder.Missing'Range loop
            if Holder.Missing (I) then
               Done := False;
               exit;
            end if;
         end loop;
         if Done then
            Complete := I;
            Log ("Packet fully reassembled", Debug, Log_Section);
         end if;

         Holder.Data
           (Holder.Data'First +
              (Max_Data_Size * (Stream_Element_Offset (Header.Index) - 1)) ..
              Stream_Element_Offset'Min
                (Holder.Data'Last,
                 Holder.Data'First +
                   (Max_Data_Size * Stream_Element_Offset (Header.Index)) - 1))
           :=
           Raw (Raw'First + 1 + Header_Octets .. Raw'Last);

         Holder.Since.Reset;
      end Assemble;

   begin
      Complete := No_Element;

      if not Has_Element (I) then
         This.Partials.Insert (Header.Seq, Create_Holder (Header), I, Ok);
         pragma Assert (Ok, "Holder insertion failed");
      end if;

      This.Partials.Update_Element (I, Assemble'Access);
   end Assemble;

end Sancta.Network.Layer.Splitter;
