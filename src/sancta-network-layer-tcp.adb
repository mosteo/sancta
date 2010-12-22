--  Implementation using regular TCP.

with Agpl.Streams.Memory_Arrays;
with Agpl.Streams;
with Agpl.Strings.Fields;
with Agpl.Tasking.Code;
with Agpl.Tasking.Workers;
--  with Agpl.Text_Io;
with Sancta.Network.Layer.Utils;

use Agpl;

package body Sancta.Network.Layer.Tcp is

   type Server_Code (Parent : Object_Access) is new Agpl.Tasking.Code.Object
   with record
      Sock : Socket_Type;
   end record;

   overriding
   procedure Run (This : in out Server_Code);

   type Listener_Code (Parent : Object_Access) is new Agpl.Tasking.Code.Object
   with record
      Sock : Socket_Type;
      Addr : Sock_Addr_Type;
   end record;

   overriding
   procedure Run (This : in out Listener_Code);

   ------------------
   -- Packet_Queue --
   ------------------

   protected body Packet_Queue is

      ---------
      -- Put --
      ---------

      procedure Put (Packet : Stream_Element_Array) is
      begin
         Packets.Append (Packet);
      end Put;

      --------------
      -- Get_Size --
      --------------

      entry Get_Size (Size : out Stream_Element_Count)
        when not Packets.Is_Empty is
      begin
         Size := Packets.First_Element'Length;
      end Get_Size;

      ---------
      -- Get --
      ---------

      entry Get (Packet : out Stream_Element_Array)
        when not Packets.Is_Empty is
      --  Should match the last Get_Size length
      begin
         if Packet'Length /= Packets.First_Element'Length then
            raise Constraint_Error with "Length mismatch:" &
            Packet'Length'Img & " /=" & Packets.First_Element'Length'Img;
         end if;
         Packet := Packets.First_Element;
         Packets.Delete_First;
      end Get;
   end Packet_Queue;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Server_Code) is
      Client : Socket_Type;
      Addr   : Sock_Addr_Type;
   begin
      Log ("Accepting connections...", Informative, Log_Section);
      loop
         begin
            Accept_Socket (This.Sock, Client, Addr);
            Log ("Accepted new connection from: " & Image (Addr),
                 Debug, Log_Section);
            Tasking.Workers.Launch
              (Listener_Code'(Parent => This.Parent,
                              Sock   => Client,
                              Addr   => Addr));
         exception
            when others =>
               Log ("Server exception: TODO: discriminate closing...",
                    Warning, Log_Section);
               delay 0.1;
         end;
      end loop;
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Listener_Code) is
      Length : Stream_Element_Count;
      Stream : Stream_Access := Gnat.Sockets.Stream (This.Sock);
   begin
      loop
         Length := Stream_Element_Count'Input (Stream);
         Log ("Incoming packet len:" & Length'Img &
              " from: " & Image (This.Addr), Debug, Log_Section);
         declare
            subtype Packet is Stream_Element_Array (1 .. Length);
            P : Packet;
         begin
            Packet'Read (Stream, P);
            Log ("Received full packet", Debug, Log_Section);
            This.Parent.Queue.Put (P);
         end;
      end loop;
   exception
      when others =>
         Agpl.Streams.Free (Agpl.Streams.Stream_Access (Stream));
         raise;
   end Run;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object;
                         Conf :        Agpl.Xml.Node)
   is
      use Agpl.Strings.Fields;
      use type Agpl.Xml.Node;

      Self : constant Agpl.Xml.Node :=
               Agpl.Xml.Get_With_Attribute
                 (Conf, "node", "id", Image (This.Id));
   begin
      if Self /= null then
         This.Port :=
           Port_Type'Value
             (Select_Field (Agpl.Xml.Get_Attribute (Self, "address"), 2, ':'));
      else
         raise Constraint_Error with "Self address not supplied";
      end if;

      Utils.Configure (This, Conf);

      Create_Socket (This.Sock, Mode => Socket_Stream);
      Set_Socket_Option
        (This.Sock,
         Socket_Level,
         (Reuse_Address, True));
      Bind_Socket   (This.Sock, (Family => Family_Inet,
                                 Addr   => Inet_Addr ("0.0.0.0"),
                                 Port   => This.Port));
      Log ("Socket bound", Informative, Log_Section);

      Listen_Socket (This.Sock);
      Log ("Socket ready to listen", Informative, Log_Section);

      This.Start_Listening;

      Tasking.Workers.Launch (Server_Code'(Parent => This.Self,
                                           Sock   => This.Sock));
   exception
      when E : others =>
         Log ("Listening socket failure while binding to port" & This.Port'Img &
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
      use Id_Socket_Maps;

      procedure Create_Dest is
         use Agpl.Strings.Fields;
         Ip    : constant String := This.Get_Address (Dest);
         Addr  : constant Sock_Addr_Type :=
                  (Family => Family_Inet,
                   Addr   => Inet_Addr (Select_Field (Ip, 1, ':')),
                   Port   => Port_Type'Value (Select_Field (Ip, 2, ':')));
         Dest_Sock : Socket_Type;
      begin
         Log ("Creating new socket for dest: " & Image (Addr),
              Debug, Log_Section);
         Create_Socket  (Dest_Sock, Mode => Socket_Stream);
--           Bind_Socket    (Dest_Sock, Addr);
         Log ("Connecting...", Debug, Log_Section);
         Connect_Socket (Dest_Sock, Addr);
         Log ("Connected", Debug, Log_Section);
         if This.Dest.Contains (Dest) then
            declare
               Sock : constant Socket_Type := This.Dest.Element (Dest);
            begin
               This.Dest.Include (Dest, Dest_Sock);
               Close_Socket (Sock);
            end;
         else
            This.Dest.Insert (Dest, Dest_Sock);
         end if;
      end Create_Dest;

   begin
      if This.Dest.Contains (Dest) then
         loop
            declare
               Size_Array  : aliased Stream_Element_Array := (1 .. 16 => 0);
               Size_Stream : aliased Agpl.Streams.Memory_Arrays.Stream_Type
                 (Size_Array'Access);

               Last        : Stream_Element_Offset;
            begin
               Log ("Sending:" & Data'Length'Img & " bytes via TCP to: " &
                    Image (Get_Peer_Name (This.Dest.Element (Dest))),
                    Debug, Log_Section);
               Stream_Element_Count'Output (Size_Stream'Access, Data'Length);
               Send_Socket (This.Dest.Element (Dest),
                            Size_Array (1 .. Size_Stream.Index) & Data,
                            Last);
               pragma Assert (Last = Data'Length + Size_Stream.Index);
               Log ("Data sent", Debug, Log_Section);
               exit;
            exception
               when E : others =>
                  Log ("Sending [known]: " & Report (E), Warning, Log_Section);
                  delay 1.0;
                  Create_Dest;
            end;
         end loop;
      else
         loop
            begin
               Create_Dest;
               This.Send (Dest, Data);
               exit;
            exception
               when E : others =>
                  Log ("Sending [new] dest: " & Image (Dest), Warning, Log_Section);
                  if This.Contains (Dest) then
                     Log ("Sending [new] IP  : " & This.Get_Address (Dest), Warning, Log_Section);
                  else
                     Log ("Sending [new] unknown IP", Warning, Log_Section);
                  end if;
                  Log ("Sending [new] except: " & Report (E),
                       Warning, Log_Section);
                  delay 1.0;
            end;
         end loop;
      end if;
   end Send;


   -------------
   -- Receive --
   -------------

   function Receive (This : Object) return Stream_Element_Array is
      Length : Stream_Element_Count;
   begin
      Log ("Blocking at incoming queue...", Debug, Log_Section);
      This.Self.Queue.Get_Size (Length);
      Log ("Got size:" & Length'Img, Debug, Log_Section);
      declare
         Buffer : Stream_Element_Array (1 .. Length);
      begin
         This.Self.Queue.Get (Buffer);
         return Buffer;
      end;
   end Receive;

end Sancta.Network.Layer.Tcp;
