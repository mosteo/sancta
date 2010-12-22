with Sancta.Component.Factory;

with Agpl.Chronos;
with Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Watchdog is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Timeout := Duration'Value
        (This.Option (Option_Timeout, Duration'Image (Default_Timeout)));

      This.Timer.Start;

      This.Subscribe (Requires_Var);

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      pragma Unreferenced (Key, Value);
   begin
      This.Timer.Reset;
   end Key_Stored;

   ----------
   -- Stop --
   ----------

   procedure Stop (This : in out Object) is
   begin
      This.Timer.Shutdown;
   end Stop;

   ----------------
   -- Timer_Task --
   ----------------

   task body Timer_Task is
      Cron : Chronos.Object;
      Done : Boolean := False;
   begin
      accept Start;
      Cron.Reset;

      while not Done loop
         select
            accept Reset;
            Cron.Reset;
         or
            accept Shutdown;
            Done := True;
         or
            delay 0.2;
         end select;

         if Cron.Elapsed > Parent.Timeout then
            Log ("Watchdog: Aborting process...", Always);
            Gnat.Os_Lib.Os_Abort;
         end if;
      end loop;
   exception
      when E : others =>
         Log ("Watchdog timer: " & Report (E), Error);
   end Timer_Task;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Watchdog;
