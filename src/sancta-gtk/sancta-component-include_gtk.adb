with Sancta.Component.Gtk_Canvas;
with Sancta.Component.Visor;

package body Sancta.Component.Include_Gtk is

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Gtk_Canvas.Register;
      Visor.Register;
   end Register;

end Sancta.Component.Include_Gtk;
