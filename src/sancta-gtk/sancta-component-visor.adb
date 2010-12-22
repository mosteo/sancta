with Agpl.Gdk.Managed;
with Agpl.Ustrings; use Agpl.Ustrings;

with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Gui.Config;
with Sancta.Gui.Visor_Data;
with Sancta.Gui.Visor_Db_View;
with Sancta.Gui.Visor_Dbv_View;
with Sancta.Gui.Visor_General_View;
with Sancta.Gui.Visor_Log_View;
with Sancta.Gui.Visor_Mission;
with Sancta.Types;

package body Sancta.Component.Visor is

   type Object_Access is access all Object;

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
     (Config : Comp_Config;
      Env    : Environment.Object)
      return Component.Object_Access
   is
   begin
      Sancta.Gui.Visor_Db_View.Register;
      Sancta.Gui.Visor_Dbv_View.Register;
      Sancta.Gui.Visor_General_View.Register;
      Sancta.Gui.Visor_Log_View.Register;
      Sancta.Gui.Visor_Mission.Register;

      declare
         This : constant Object_Access := new Object (Name'Access, Config);
         Opts :          Gui.Config.Object;

         procedure Create_Gtk is
         begin
            This.Visor :=
              new Gui.Visor.Object
                (Network.Network (This.Input (Requires_Link)).Link);
            This.Tracer := new Gui.Visor.Trace.Object (This.Visor);

            This.Period.Set_Period
              (Duration'Value (This.Option (Option_Period, Default_Period'Img)));

            This.Tracer.Set_Level (Debug);
            Agpl.Trace.Add_Tracer (Agpl.Trace.Object_Access (This.Tracer));

            Opts :=
              (Id           => Env.Id,
               Laser_Scans  => Natural'Value
                 (This.Option (Option_Laser_Scans, Default_Laser_Scans'Img)),
               Laser_Range  => Types.Real'Value
                 (This.Option (Option_Laser_Range, Default_Laser_Range'Img)),
               Mission_File => +Default_Mission,
               Draw_Grid    => Boolean'Value
                 (This.Option (Option_Draw_Grid, Default_Draw_Grid'Img)),
               Show_Poses   => Boolean'Value
                 (This.Option (Option_Show_Poses, Default_Show_Poses'Img)),
               Show_Mission => False
              );

            This.Visor.Create_Simple (Opts);
         end Create_Gtk;

      begin
         Agpl.Gdk.Managed.Execute (Create_Gtk'Access);

         Log ("Visor ready", Informative);

         This.Output
           (Provides_Drawer,
            Ctypes.Drawer'
              (Drawer => Gui.Visor_Data.Drawer (This.Visor.Get_Data)));

         return Component.Object_Access (This);
      end;
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      This.Visor.Run;
      This.Period.Next (Next);
   end Run;

end Sancta.Component.Visor;
