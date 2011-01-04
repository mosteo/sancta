with Agpl.Drawing.Figures;
with Agpl.Gdk.Managed.Drawing_Area;
with Gtk.Widget;    use Gtk.Widget;
with Sancta.Ctree.CTTypes;
with Sancta;        use Sancta;
with Sancta.Ctree;  use Sancta.Ctree;

package Sancta.Gtk.Link_History is

   Log_Section : constant String := "nerus.gtk.link_history";

   type Object (Samples : Positive) is tagged private;

   type Object_Access is access all Object;

   not overriding
   procedure Create (This     : in out Object;
                     Threshold: Signal_Q;
                     From, To : Node_Id;
                     Widget    :    out Gtk_Widget;
                     Bgcolor  : String := "white");

   not overriding
   procedure Update (This : in out Object;
                     Data :        CTtypes.Full_Signal'Class);

private

   type Object (Samples : Positive) is tagged record
      From,
      To    : Node_Id;

      Threshold : Signal_Q;

      Area  : Agpl.Gdk.Managed.Drawing_Area.Handle;
      Hist  : Agpl.Drawing.Figures.Float_List;
   end record;

end Sancta.Gtk.Link_History;
