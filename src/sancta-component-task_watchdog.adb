with Sancta.Component.Factory,
     Sancta.Component.Types;

package body Sancta.Component.Task_Watchdog is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Tasks);
      return Component.Object_Access (This);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
   begin
      This.Output
        (Provides_Flag,
         Types.Bool'(Data with Types.Task_List (Value).Tasks.Is_Empty));
   end Key_Stored;

end Sancta.Component.Task_Watchdog;
