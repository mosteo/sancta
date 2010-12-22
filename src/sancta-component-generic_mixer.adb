with Sancta.Component.Factory;

package body Sancta.Component.Generic_Mixer is

   ---------
   -- Mix --
   ---------

   function Mix
     (This : not null access Object;
      X, Y :                 Data'Class)
      return Data'Class
   is
      pragma Unreferenced (This, X);
   begin
      return Y;
   end Mix;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Object_Preparer) is
   begin
      This.Parent.Subscribe (Requires_X);
      This.Parent.Subscribe (Requires_Y);
   end Initialize;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
   begin
      if Key = Requires_X then
         if This.Exists (Requires_Y) then
            This.Output (Provides_Z, This.Mix (Value, This.Input (Requires_Y)));
         else
            This.Output (Provides_Z, Value);
         end if;
      else
         if This.Exists (Requires_X) then
            This.Output (Provides_Z, This.Mix (This.Input (Requires_X), Value));
         else
            This.Output (Provides_Z, Value);
         end if;
      end if;
   end Key_Stored;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
   begin
      return new Object (Name'Access, Config);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

end Sancta.Component.Generic_Mixer;
