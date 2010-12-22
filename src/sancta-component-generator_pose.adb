with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Convert;

package body Sancta.Component.Generator_Pose is

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   -------------
   -- To_Data --
   -------------

   function To_Data
     (This : Object;
      Val : String)
      return Data'Class
   is
      pragma Unreferenced (This);
   begin
      return Ctypes.Pose'(Pose => Convert.To_Pose (Val));
   end To_Data;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
   begin
      return new Object (Name'Access, Config);
   end Create;

end Sancta.Component.Generator_Pose;
