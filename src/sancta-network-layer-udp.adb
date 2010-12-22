--  Implementation using regular UDP.

with Agpl.Strings.Fields,
     Agpl.Text_Io,
     Sancta.Network.Layer.Utils;

use Agpl,
    Agpl.Text_Io;

package body Sancta.Network.Layer.Udp is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node)
   is
      use Agpl.Strings.Fields;
   begin
      Utils.Configure (This, Conf);
      This.Initialize
        (Port =>
           Integer'Value
             (Select_Field
                  (Xml.Get_Attribute
                       (Xml.Get_With_Attribute
                            (Conf, "node", "id", Image (This.Id)),
                        "address",
                        "0"),
                   2,
                   ':')));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object;
                         Port :        Positive)
   is
      --  Create our listening socket
   begin
      This.Port := Port_Type (Port);

      Create_Socket (This.Sock, Mode => Socket_Datagram);
      Bind_Socket   (This.Sock, (Family => Family_Inet,
                                 Addr   => Inet_Addr ("0.0.0.0"),
                                 Port   => This.Port));

      Log ("Socket bound", Informative, Log_Section);

      This.Start_Listening;
   exception
      when E : others =>
         Log ("Listening socket failure while binding to port" & Port'Img &
              ": " & Report (E), Error);
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
   begin
      Root.Object (This).Shutdown;

      begin
         Shutdown_Socket (This.Sock, Shut_Read);
      exception
         when E : Socket_Error =>
            if Resolve_Exception (E) = Transport_Endpoint_Not_Connected or else
               Resolve_Exception (E) = Socket_Operation_On_Non_Socket or else
               True -- HUM?
            then
               null;
               --  This is necessary because a Receive will not fail if already
               --  started in a datagram socket, even if the socket is subse-
               --  quently closed.
            else
               raise;
            end if;
      end;
      begin
         Close_Socket (This.Sock);
         --  Closing the socket should abort the listening call in the receiver.
      exception
         when others =>
            null;
      end;
   end Shutdown;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out Object;
                   Dest : in     Node_Id;
                   Data : in     Stream_Element_Array)
   is
      use Agpl.Strings.Fields;
      Last : Stream_Element_Offset;

      Ip   : constant String := This.Get_Address (Dest);
      Addr : constant Sock_Addr_Type :=
               (Family => Family_Inet,
                Addr   => Inet_Addr (Select_Field (Ip, 1, ':')),
                Port   => Port_Type'Value (Select_Field (Ip, 2, ':')));
   begin
      Log ("Sending" & Data'Length'Img & " bytes to " & Image (Dest) &
           " (" & Ip & ")",
           Debug, Section => Log_Section);

      Send_Socket (This.Sock,
                   Data,
                   Last,
                   Addr);
      if Last /= Data'Last then
         raise Program_Error with "Data couldn't be sent in one piece!";
      end if;
   end Send;


   -------------
   -- Receive --
   -------------

   function Receive (This : Object) return Stream_Element_Array is
      Buffer : Stream_Element_Array (1 .. Max_Message_Size);
      Last   : Stream_Element_Offset;
      Sender : Sock_Addr_Type;
   begin
      Receive_Socket (This.Sock, Buffer, Last, Sender);
      Log ("Layer.Udp: Something incoming from " &
           Gnat.Sockets.Image (This.Sock), Debug, Log_Section);

      --  Exit when no more data:
      if Last < Buffer'First then
         raise Root.Shutting_Down;
      else
         return Buffer (Buffer'First .. Last);
      end if;

   exception
      when E : Socket_Error =>
         if Resolve_Exception (E) = Transport_Endpoint_Not_Connected or else
            Resolve_Exception (E) = Socket_Operation_On_Non_Socket
         then
            raise Root.Shutting_Down;
         else
            Log ("Layer.Udp [Socket error]: " & Report (E), Error, Log_Section);
            raise Root.Interrupted_Reception;
         end if;
      when E : others =>
         Log ("Layer.Udp [others]: " & Report (E), Error, Log_Section);
         Put_Line ("Fatal");
         raise Root.Interrupted_Reception;
   end Receive;

end Sancta.Network.Layer.Udp;
