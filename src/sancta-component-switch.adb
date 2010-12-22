with Sancta.Component.Ctypes;
with Sancta.Component.Factory;

package body Sancta.Component.Switch is

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
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Switcher);
      This.Subscribe (Requires_When_Off);
      This.Subscribe (Requires_When_On);

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      Bool_Status_Map : constant array (Boolean) of States :=
        (False => Off,
         True  => On);
   begin
      if Key = Requires_Switcher then
         if Value in Ctypes.Bool'Class then
            This.Status := Bool_Status_Map (Ctypes.Bool (Value).Value);
         else
            This.Status := On;
            This.Unsubscribe (Requires_When_Off);
            --  We'll never go back to Off status...
         end if;
      elsif Key = Requires_When_Off then
         if This.Status = Off then
            This.Output (Provides_Output, Value);
         end if;
      elsif Key = Requires_When_On then
         if This.Status = On then
            This.Output (Provides_Output, Value);
         end if;
      else
         raise Program_Error;
      end if;
   end Key_Stored;

   --------------------
   -- Is_Thread_Safe --
   --------------------

   function Is_Thread_Safe (This : Object) return Boolean is
      pragma Unreferenced (This);
   begin
      return True;
   end Is_Thread_Safe;

end Sancta.Component.Switch;
