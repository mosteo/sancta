with Sancta.Gui.Canvas;
with Sancta.Gui.Main;
with Sancta.Gui.Test_Pkg;

with Gtk.Drawing_Area;
with Gtk.Main;
with Gtk.Widget;
with Gtkada.Handlers; use Gtkada.Handlers;

procedure Sancta.Gui.Test1 is
   Draw1 : Gtk.Drawing_Area.Gtk_Drawing_Area;
   Draw2 : Gtk.Drawing_Area.Gtk_Drawing_Area;
begin
   Gtk.Main.Init;

   Gtk.Drawing_Area.Gtk_New (Draw1);
   Gtk.Drawing_Area.Gtk_New (Draw2);

   Return_Callback.Connect
     (Draw1,
      "expose_event",
      Return_Callback.To_Marshaller (Test_Pkg.Expose_Handler'Access));

   Return_Callback.Connect
     (Draw2,
      "expose_event",
      Return_Callback.To_Marshaller (Test_Pkg.Expose_Handler2'Access));

   declare
      Hello : Gui.Canvas.Object :=
                Gui.Canvas.Create ("Hello", Gtk.Widget.Gtk_Widget (Draw1));
      Gbye  : Gui.Canvas.Object :=
                Gui.Canvas.Create ("Good bye!", Gtk.Widget.Gtk_Widget (Draw2));
   begin
      Gui.Main.Start;
      Gui.Main.Thread.Add_Canvas (Hello);
      Gui.Main.Thread.Add_Canvas (Gbye);
   end;
end Sancta.Gui.Test1;
