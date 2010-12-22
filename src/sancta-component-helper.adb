with Sancta.Component.Factory;

package body Sancta.Component.Helper is

   ------------
   -- Create --
   ------------

   function Create (Config : in Comp_Config) return Object is
   begin
      return This : Object (Name'Access, Config) do
         pragma Unreferenced (This);
         null;
      end return;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Config : in Comp_Config) return Component.Object_Access is
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

   -----------
   -- Input --
   -----------

   function Input (Config : Comp_Config;
                   Key    : Internal_Key) return Data'Class
   is
      Help : constant Object := Create (Config);
      --  Spureous warning here?
   begin
      return Help.Input (Key);
   end Input;

   ------------
   -- Option --
   ------------

   function Option (Config : Comp_Config;
                    Attr   : Option_Attr) return String
   is
      Help : constant Object := Create (Config);
      --  Spureous warning here?
   begin
      return Help.Option (Attr);
   end Option;

   ------------
   -- Option --
   ------------

   function Option (Config : Comp_Config;
                    Attr   : Option_Attr;
                    Def    : String) return String
   is
      Help : constant Object := Create (Config);
      --  Spureous warning here?
   begin
      return Help.Option (Attr, Def);
   end Option;

end Sancta.Component.Helper;
