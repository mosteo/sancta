with Player.Client,
     Sancta.Component.Utils;

package body Sancta.Component.Player_Iface is

   ------------
   -- Client --
   ------------

   function Client (This : Object) return Player_Client.Safe_Access is
   begin
      return This.Cached_Client;
   end Client;

   --------------------------
   -- Create_And_Subscribe --
   --------------------------

   procedure Create_And_Subscribe (This  : in out Object)
   is
      use Player_Client,
          Utils;

      type Safe_Code is new Player_Code with null record;
      overriding procedure Run (Code : in out Safe_Code;
                                Conn : in out Player.Client.Object)
      is
         pragma Unreferenced (Code);
      begin
         Log ("Creating Player interface: " & External_Tag (This.Iface'Tag),
              Always, Log_Section);
         This.Iface.Create    (Conn, Option (This, Option_Index, 0));
         Log ("Subscribing Player interface: " & External_Tag (This.Iface'Tag),
              Always, Log_Section);
         This.Iface.Subscribe (Player.Open_Mode);
         Log ("Done Player interface: " & External_Tag (This.Iface'Tag),
              Always, Log_Section);
      end Run;

      Code : Safe_Code;
   begin
      This.Cached_Client := Connection (This.Input (Requires_Client)).Ref;
      This.Cached_Client.all.Execute (Code);
   end Create_And_Subscribe;

   -----------
   -- Index --
   -----------

   function Index (This : Object) return Natural is
   begin
      return Natural'Value (This.Option (Option_Index, "0"));
   end Index;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (This : in out Object)
   is
   begin
      This.Iface.Unsubscribe;
      This.Iface.Destroy;
   end Stop;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object_Preparer) is
   begin
      This.Parent.Iface := Object'Class (This.Parent.all).Create_Interface;
      This.Parent.Create_And_Subscribe;
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : Object;
                      Code : access procedure
                        (Iface : Player_Client.Iface_Access))
   is
   begin
      This.Cached_Client.all.Execute (Code, This.Iface);
   end Execute;

   --------------------
   -- Requires_Mutex --
   --------------------

   function Requires_Mutex (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return False;
   end Requires_Mutex;

end Sancta.Component.Player_Iface;
