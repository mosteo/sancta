private with Agpl.Drawing.Multisource;
private with Agpl.Gdk.Managed.Drawing_Area;
private with Sancta.Component.Root;

package Sancta.Component.Gtk_Canvas is

   Log_Section : constant String := "sancta.component.gtk_canvas";

   Name : aliased constant Component_Name := "gtk_canvas";

   Requires_Drawable : constant Internal_Key := "drawable";
   --  This must be either a drawable or a parcel holding one.
   --  Refresh isn't periodic, but triggered by key storing!
   Requires_Handler  : constant Internal_Key := "handler";
   --  An Agpl.Gui.Event_Handler to be attached for interaction.
   --  NOTE: these two above could be the same, if you know what you're doing!

   Opt_Title : constant Option_Attr := "title";
   Def_Title : constant String      := "SANCTA canvas";

   procedure Register;

private

   type Object is new Root.Object with record
      Sources : Agpl.Drawing.Multisource.Object;
      Canvas  : Agpl.Gdk.Managed.Drawing_Area.Handle;
   end record;


   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

--     function Create (Config : Comp_Config;
--                      Env    : Environment.Object)
--                      return   Component.Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

--     overriding
--     procedure Run (This : in out Object;
--                    Next :    out Ada.Calendar.Time);

--     overriding
--     procedure Stop (This : in out Object);

end Sancta.Component.Gtk_Canvas;
