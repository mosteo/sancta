with Sancta.Component.Factory;
with Sancta.Component.Utils;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Component.Player_Client is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      use Agpl.Xml,
          Utils;
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Conn.Create
        (Host => Option (This.all, Option_Host, "127.0.0.1"),
         Port => Option (This.all, Option_Port, 6665));

      This.Conn.Connect;
      This.Conn.Datamode (Player.Client.Datamode_Pull);
      This.Conn.Set_Replace_Rule (-1, -1, Player.Msgtype_Data, -1, True);
      This.Conn.Set_Request_Timeout (60);

      This.Output (Provides_Connection,
                   Connection'(Ref => This.Safe'Access));

      Log ("Connected with player successfully", Informative, Log_Section);

      if This.Exists (Option_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Option_Period)));
      end if;

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
   begin
      This.Safe.Sync;

      This.Period.Next (Next);
   end Run;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      This.Conn.Disconnect;
   end Stop;

   -----------------
   -- Safe_Object --
   -----------------

   protected body Safe_Object is

      ----------
      -- Sync --
      ----------

      procedure Sync is
      begin
--           Log ("XXX", Always);
         This.Conn.Read;
--           Log ("YYY", Always);
      end Sync;

      -------------
      -- Execute --
      -------------

      procedure Execute (Code : in out Player_Code'Class) is
      begin
         Code.Run (This.Conn);
      end Execute;

      -------------
      -- Execute --
      -------------

      procedure Execute (Code : access procedure) is
      begin
         Code.all;
      end Execute;

      -------------
      -- Execute --
      -------------

      procedure Execute (Code  : access procedure (Iface : Iface_Access);
                         Iface : Iface_Access) is
      begin
         Code (Iface);
      end Execute;

   end Safe_Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   --------------------
   -- Requires_Mutex --
   --------------------

   function Requires_Mutex (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return False;
   end Requires_Mutex;

end Sancta.Component.Player_Client;
