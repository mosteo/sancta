package body Sancta.Gui.Visor_Factory is

   use Creator_Maps;

   --------------
   -- Register --
   --------------

   procedure Register (This : in Creator;
                       Name : in String)
   is
   begin
      Creators.Insert (Name, This);
   end Register;

end Sancta.Gui.Visor_Factory;
