with Agpl.Drawing.Buffer;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;

package body Sancta.Gtk.Link_History is

   ------------
   -- Create --
   ------------

   procedure Create
     (This      : in out Object;
      Threshold :        Signal_Q;
      From, To  :        Node_Id;
      Widget    :    out Gtk_Widget;
      Bgcolor   : String := "white")
   is
      HB : Gtk_HBox;
      L  : Gtk_Label;
      procedure Attach (W : Gtk_Widget) is
      begin
         W.Set_Size_Request (8, 8); -- Avoids some silly crash in GTK
         HB.Pack_Start (W, Expand => True);
      end Attach;
   begin
      This.From := From;
      This.To   := To;

      This.Threshold := Threshold;

      Gtk_New_HBox (HB, Spacing => 2);
      Gtk_New (L, "<tt>" & Image (From) & "--" & Image (To) & "</tt>");
      L.Set_Use_Markup (True);
      HB.Pack_Start (L, Expand => False);

      This.Area :=
        Agpl.Gdk.Managed.Drawing_Area.Show
          (Attach'Access, Bgcolor => Bgcolor, Square => False);

      Widget := Gtk_Widget (HB);
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update
     (This : in out Object;
      Data :        CTTypes.Full_Signal'Class)
   is
      B : Agpl.Drawing.Buffer.object;
   begin
      This.Hist.Add_Sample (Float (Data.Links.Get (This.From, This.To)));
      This.Hist.Trim (This.Samples);

      B.Set_Color ((255, 0, 0), 0);
      B.Draw_Line (0.0,                  Float (This.Threshold),
                   Float (This.Samples), Float (This.Threshold));

      Agpl.Drawing.Figures.Simple_History
        (Float (Signal_Q'First),
         Float (Signal_Q'Last),
         This.Hist,
         This.Samples).Draw (B);

      This.Area.Draw (B);
   exception
      when E : others =>
         Log ("Update: " & Report (E), Error, Log_Section);
   end Update;

end Sancta.Gtk.Link_History;
