with Agpl.Drawing.Buffer;
with Agpl.Drawing.Figures;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Gtk.User_Data;
with Gtk.Box;   use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Sancta.Ctree;
with Sancta.Types;

package body Sancta.Gtk.Ctree_Node_Status is

   Widget_Pose   : constant String := "pose";
   Widget_Report : constant String := "report";
   Widget_Q      : constant String := "Q";

   Q_Samples     : constant := 300;

   type Persistent_Data is record
      Qs       : Agpl.Drawing.Figures.Float_List;
      Q_Canvas : Agpl.Gdk.Managed.Drawing_Area.Handle;
   end record;
   type Persistent_Access is access all Persistent_Data;

   package Gtk_Persistent_Data is
      new Agpl.Gtk.User_Data (Persistent_Access);

   ------------
   -- Create --
   ------------

   function Create
     (This : Object)
      return Agpl.Gdk.Widget_Bundle.Object
   is
      pragma Unreferenced (This);
      B : Agpl.Gdk.Widget_Bundle.Object;
      L : Gtk_Label;
      V : Gtk_VBox;
      H : Gtk_Hbox;

      P : constant Persistent_Access := new Persistent_Data;
   begin
      Gtk_New_Vbox (V, True, 0);
      Gtk_Persistent_Data.Set (V, P);
      B.Set_Root (V);

      Gtk_New (L, "Pose unknown");
      L.Set_Use_Markup (True);
      L.Set_Selectable (True);
      L.Set_Size_Request (1);
      L.Set_Alignment (0.0, 0.5);
      V.Pack_Start (L);
      B.Set (Widget_Pose, L);

      Gtk_New (L, "Status unknown");
      L.Set_Use_Markup (True);
      L.Set_Selectable (True);
      L.Set_Size_Request (1);
      L.Set_Alignment (0.0, 0.5);
      V.Pack_Start (L);
      B.Set (Widget_Report, L);

      Gtk_New_Hbox (H, Homogeneous => False, Spacing => 2);
      V.Pack_Start (H, Expand => True);

      Gtk_New (L, "PredQ [???.??]");
      L.Set_Use_Markup (True);
      L.Set_Selectable (True);
      H.Pack_Start (L, Expand => False);
      B.Set (Widget_Q, L);

      declare
         procedure Attach (W : Gtk_Widget) is
         begin
            W.Set_Size_Request (8, 8);
            H.Pack_Start (W, Expand => True, Fill => True);
         end Attach;
      begin
         P.Q_Canvas :=
           Agpl.Gdk.Managed.Drawing_Area.Show (Attach'Access, Square => False);
      end;

      return B;
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update
     (This : in out Object;
      Guts : in out Agpl.Gdk.Widget_Bundle.Object)
   is
      L : Gtk_Label;
      P : constant Persistent_Access :=
            Gtk_Persistent_Data.Get (Guts.Get_Root);
   begin
      L := Gtk_Label (Guts.Get (Widget_Pose));
      L.Set_Label
        ("Pose [<b>" &
         Sancta.Types.Image (This.Pose) & "</b>]");

      L := Gtk_Label (Guts.Get (Widget_Report));
      L.Set_Label
        ("Report [<b>" & This.Report & "</b>]");

      L := Gtk_Label (Guts.Get (Widget_Q));
      L.Set_Label
        ("PredQ [<b>" & This.Q'Img & "</b>]");

      --  Draw signal history
      declare
         use Agpl.Drawing;
         use Sancta.Ctree;
         Hist : Agpl.Drawing.Buffer.Object;
      begin
         P.Qs.Append (Float (This.Q));
         P.Qs.Trim (Q_Samples);

         Hist.Set_Color ((255, 0, 0), 0);
         Hist.Draw_Line (0.0,               Float (This.Threshold),
                         Float (Q_Samples), Float (This.Threshold));
         Figures.Simple_History
           (Float (Signal_Q'First),
            Float (Signal_Q'Last),
            P.Qs, Q_Samples).Draw (Hist);
         P.Q_Canvas.Draw (Hist);
      end;
   end Update;

   ----------
   -- Cast --
   ----------

   function Cast
     (From : Sancta.Ctree.Distributed.Node_Status)
      return Object
   is
   begin
      return Object'(From with null record);
   end Cast;

end Sancta.Gtk.Ctree_Node_Status;
