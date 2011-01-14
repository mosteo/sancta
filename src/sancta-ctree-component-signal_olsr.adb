with Ada.Streams;
with Agpl.Conversions;
with Agpl.Socket_Utils;
with Agpl.Streams.Memory_Arrays;
with Agpl.Strings;
with Agpl.Strings.Fields;
with Agpl.Xml;
with Gnat.Sockets;
with Sancta.Ctree.Nctypes;
with Sancta.Component.Factory;

package body Sancta.Ctree.Component.Signal_Olsr is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config;
      Env    : Environment.Object)
      return Sancta.Component.Object_Access
   is
      use Agpl;
      This : constant Object_Access := new Object (Name'Access, Config);

      Nodes : constant Xml.Node_Vector := Xml.Get_All (Config, "node");
   begin
      This.Id := Env.Id;

      for I in Nodes.First_Index .. Nodes.Last_Index loop
         declare
            Ip : constant String :=
                   Xml.Get_Attribute (Nodes.Element (I), "address");
            Id : constant String :=
                   Xml.Get_Attribute (Nodes.Element (I), "id");
         begin
            pragma Assert (Xml.Has_Attribute (Nodes.Element (I), "id"));
            pragma Assert (Xml.Has_Attribute (Nodes.Element (I), "address"));

            This.Ip_Id.Insert (Ip, Id);

            Log ("Added: " & Id & " <--> " & Ip, Debug, Log_Section);

            This.Zero_Links.Insert (Sancta.Value (Id), 0.0);
         end;
      end loop;

      if This.Exists (Opt_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Opt_Period)));
      end if;

      if This.Exists (Opt_Txtport) then
         This.Port := Natural'Value (This.Option (Opt_Txtport));
      end if;

--        Gnat.Sockets.Initialize;

      return Sancta.Component.Object_Access (This);
   end Create;

   -------------
   -- Process --
   -------------

   procedure Process (This : in out Object) is
      use Gnat.Sockets;
      Addr : Sock_Addr_Type;
      Sock : Socket_Type;
      Hello : aliased
        Ada.Streams.Stream_Element_Array := (1 .. 10 => 0);
      Mem  : aliased Agpl.Streams.Memory_Arrays.Stream_Type (Hello'Access);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Addr.Addr := Inet_Addr ("127.0.0.1");
      Addr.Port := Port_Type (This.Port);

      Log ("Addr prepared", Debug, Det_Section);
      Create_Socket (Sock);
      Log ("Socket created", Debug, Det_Section);
      Connect_Socket (Sock, Addr);
      Log ("Socket connected", Debug, Det_Section);

      String'Write (Mem'Access, "/topology" & Character'Val (10));
      Send_Socket (Sock, Hello (Hello'First .. Mem.Index), Last);
      Log ("Hello sent", Debug, Det_Section);

      This.Links := This.Zero_Links;

      loop
         declare
            use Agpl.Strings;
            Line : constant String := Agpl.Socket_Utils.Read_Line (Sock);
         begin
--              Log ("Line: " & Line, Debug, Log_Section);
            if Line = "Table: Topology" then
               Log ("Trigger found", Debug, Det_Section);
               Agpl.Socket_Utils.Skip_Line (Sock);
               loop
                  declare
                     Link : constant String :=
                              Agpl.Socket_Utils.Read_Line (Sock);
                     T    : constant Character := Character'Val (9); --  Tab
                  begin
                     if Link'Length < 4 then
                        Log ("End of topology info", Debug, Det_Section);
                        Shutdown_Socket (Sock);
                        Close_Socket (Sock);
                        return;
                     else
                        declare
                           Ip : constant String :=
                                  Fields.Select_Field (Link, 1, T);
                           Q  : constant String :=
                                  Fields.Select_Field (Link, 3, T);
                           IQ : constant String :=
                                  Fields.Select_Field (Link, 4, T);
                           --  Inverse Q?
                        begin
                           Log ("Link info: " & Link, Debug, Det_Section);
                           Log ("IP: " & Ip, Debug, Det_Section);
                           Log ("Q : " & Q,  Debug, Det_Section);
                           Log ("IQ: " & IQ,  Debug, Det_Section);
                           if This.Ip_Id.Contains (Ip) then
                              This.Add_Link
                                (Sancta.Value (This.Ip_Id.Element (ip)),
                                 Signal_Q'Min
                                   (To_Signal_Q (100.0 * Float'Value (Q)),
                                    To_Signal_Q (100.0 * Float'Value (IQ))));
                           else
                              Log ("Dropping info for unknown host.",
                                   Warning, Log_Section);
                           end if;
                        end;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   exception
      when E : others =>
         Close_Socket (Sock);
         Log ("Process: " & Report (E), Error, Log_Section);
   end Process;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Agpl.Conversions;
   begin
      Process (This);

      This.Links.Exclude (This.Id);

      declare
         use Ctree.Id_Q_Maps;
         procedure Print (I : Cursor) is
         begin
            Log (Sancta.Image (Key (I)) & ": " & To_String (Float (Element (I))),
                 Debug, Log_Section);
         end Print;
      begin
         This.Links.Iterate (Print'Access);
      end;

      This.Output (Provides_Signal,
                   Nctypes.Signal'(Links => This.Links));

      This.Period.Next (Next);
   end Run;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (This     : in out Object;
      Other_Id :        Node_Id;
      Other_Q  :        Signal_Q)
   is


   begin
--        Log ("Link: " & Sancta.Image (Other_Id) & " " & To_String (Float (Other_Q)),
--             Debug, Log_Section);
      This.Links.Include (Other_Id, Other_Q);
   end Add_Link;

end Sancta.Ctree.Component.Signal_Olsr;
