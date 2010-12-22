--  Sample empty component to save-as when creating new ones.

with Agpl.Tasking.Period;
with Sancta.Component.Root;

private with Sancta.Component.Environment;
private with Sancta.Gui.Visor;
private with Sancta.Gui.Visor.Trace;

package Sancta.Component.Visor is

   Log_Section : constant String := "sancta.component.visor";

   Name : aliased constant Component_Name := "visor";

   Requires_Link         : constant Internal_Key := "link";
   Provides_Drawer       : constant Internal_Key := "drawer";
   --  Sancta.Component.Types.Drawer

   Option_Mission      : constant Option_Attr  := "mission";
   Default_Mission     : constant String       := "mission.xml";
   --  File with tasks
   Option_Laser_Scans  : constant Option_Attr  := "laser_scans";
   Default_Laser_Scans : constant := 4;
   --  Scans to remember and draw
   Option_Laser_Range  : constant Option_Attr  := "laser_range";
   Default_Laser_Range : constant              := 31.0;
   --  Max laser range
   Option_Draw_Grid    : constant Option_Attr  := "draw_grid";
   Default_Draw_Grid   : constant Boolean      := True;
   Option_Show_Poses   : constant Option_Attr  := "show_poses";
   Default_Show_Poses  : constant Boolean      := True;
   Option_Period       : constant Option_Attr  := "period";
   Default_Period      : constant Duration     := 0.01;

   procedure Register;

private

   use Agpl;

   type Object is new Root.Object with record
      Visor  : Gui.Visor.Object_Access;
      Tracer : Gui.Visor.Trace.Object_Access;
      Period : Tasking.Period.Object := Tasking.Period.Create (Default_Period);
   end record;

   function Create (Config : Comp_Config;
                    Env    : Environment.Object)
                    return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

--     overriding
--     procedure Stop (This : in out Object);

end Sancta.Component.Visor;
