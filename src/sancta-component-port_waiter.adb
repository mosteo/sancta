with Agpl.Chronos;

with Gnat.Sockets;

with Sancta.Component.Factory;
with Sancta.Component.Ctypes;

package body Sancta.Component.Port_Waiter is

   type Object_Access is access all Object;

   task type Waiter_Type (This : Object_Access;
                          Port : Natural) is
      entry Set_Host (Host : String);
      entry Done;
   end Waiter_Type;

   task body Waiter_Type is
      use Gnat.Sockets;
      Addr : Sock_Addr_Type;
      Sock : Socket_Type;
      Cron : Agpl.Chronos.Object;
   begin
      accept Set_Host (Host : String) do
         Addr.Addr := Inet_Addr (Host);
         Addr.Port := Port_Type (Port);
      end Set_Host;
      Cron.Reset;
      Create_Socket (Sock);

      loop
         declare
            Attempt : Positive := 1;
         begin
            Connect_Socket (Sock, Addr);
            --  Success!
            This.Output (Provides_Flag, Ctypes.Bool'(Value => True));
            Log ("Port" & Port'Img & " open after " & Cron.Image,
                 Informative, Log_Section);
            Shutdown_Socket (Sock);
            Close_Socket (Sock);
            exit;
         exception
            when E : Socket_Error =>
               case Resolve_Exception (E) is
                  when Connection_Refused | Connection_Timed_Out =>
                     Log ("Knock, knock..." & Attempt'Img, Debug, Log_Section);
                     delay 1.0;
                  when Connection_Reset_By_Peer =>
                     --  ? Port is open but closes on us
                     raise Program_Error;
                  when others =>
                     Log ("Knocking: " & Report (E), Warning, Log_Section);
                     delay 1.0;
               end case;
               Attempt := Attempt + 1;
         end;
      end loop;

      select
         accept Done;
      or
         terminate;
      end select;
   end Waiter_Type;

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
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This   : constant Object_Access := new Object (Name'Access, Config);
      Waiter : constant access Waiter_Type :=
                 new Waiter_Type (This, Natural'Value (This.Option (Opt_Port)));
   begin
      Waiter.Set_Host (This.Option (Opt_Host, Def_Host));

      if Boolean'Value (This.Option (Opt_Wait, Def_Wait'Img)) then
         Waiter.Done;
         Log ("Port has been opened", Debug, Log_Section);
      end if;

      return Component.Object_Access (This);
   end Create;

end Sancta.Component.Port_Waiter;
