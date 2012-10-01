with Sancta.Component.Factory;

package body Sancta.Component.Copier is

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
      This.Subscribe (Requires_X);

      This.Wrap := Boolean'Value (This.Option (Opt_Wrap, Def_Wrap'Img));
      if This.Wrap then
         This.Label := +This.Option (Opt_Label);
      end if;

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
      pragma Unreferenced (Key);
   begin
      if This.Wrap then
         This.Output (Provides_Y, Wrap (Value, +This.Label, This.Get_Id));
      else
         This.Output (Provides_Y, Value);
      end if;
   end Key_Stored;

end Sancta.Component.Copier;
