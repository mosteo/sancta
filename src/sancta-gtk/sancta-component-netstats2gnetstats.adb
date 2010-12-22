with Agpl.Conversions;
with Agpl.Drawing.Figures;
with Agpl.Gdk.Managed.Drawing_Area;
with Agpl.Gtk.User_Data;
with Agpl.Strings;
with Gtk.Box;                            use Gtk.Box;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Label;                          use Gtk.Label;
with Gtk.Table;                          use Gtk.Table;
with Gtk.Widget;                         use Gtk.Widget;

package body Sancta.Component.Netstats2Gnetstats is

   ----------
   -- Cast --
   ----------

   function Cast (From : Netstats.Stats) return Stats is
   begin
      return Stats'(From with null record);
   end Cast;

   type Extra_Data is record
      Ping  : Agpl.Drawing.Figures.Float_List;
      BWin  : Agpl.Drawing.Figures.Float_List;
      BWout : Agpl.Drawing.Figures.Float_List;

      H_Ping,
      H_BWin,
      H_BWout : Agpl.Gdk.Managed.Drawing_Area.Handle;
   end record;

   type Extra_Access is access all Extra_Data;

   package Extras is new Agpl.Gtk.User_Data (Extra_Access);

   Label_Ping : constant String := "label_ping";
   Label_BWin : constant String := "label_bwin";
   Label_BWout : constant String := "label_bwou";

   Samples : constant := 60;

   ------------
   -- Create --
   ------------

   function Create
     (This : Stats)
      return Agpl.Gdk.Widget_Bundle.Object
   is
      Guts  : Agpl.Gdk.Widget_Bundle.Object;

      Extra : constant Extra_Access := new Extra_Data;

      Box   : Gtk_VBox;
   begin
      --  Root VBox
      Gtk_New_VBox (Box, Homogeneous => False);
      Guts.Set_Root (Box);
      Extras.Set (Box, Extra);

      --  Ping Box, Label, Hist:
      if This.With_Ping then
         declare
            HB : Gtk_HBox;
            L  : Gtk_Label;
            procedure Attach (W : Gtk_Widget) is
            begin
               HB.Pack_Start (W, Expand => True);
            end Attach;
         begin
            Gtk_New_HBox (HB, Homogeneous => False, Spacing => 2);
            Box.Pack_Start (HB, Expand => False);
            Gtk_New (L, "Ping: ????ms");
            L.Set_Use_Markup (True);
            HB.Pack_Start (L, Expand => False);
            Extra.H_Ping := Agpl.Gdk.Managed.Drawing_Area.Show
              (Attach'Access, "azure", Square => False);
            Guts.Set (Label_Ping, L);
         end;
      end if;

      --  BW label, boxes
      declare
         Tbl : Gtk_Table;
         L   : Gtk_Label;
         procedure Attach_In (W : Gtk_Widget) is
         begin
            Tbl.Attach (W, 1, 2, 0, 1);
         end Attach_In;
         procedure Attach_Out (W : Gtk_Widget) is
         begin
            Tbl.Attach (W, 1, 2, 1, 2);
         end Attach_Out;
      begin
         Gtk_New (Tbl, 2, 2, Homogeneous => False);
         Box.Pack_Start (Tbl, Expand => False);

         Gtk_New (L, "BW in: ?");
         L.Set_Use_Markup (True);
         Tbl.Attach (L, 0, 1, 0, 1, Xoptions => Shrink);
         Guts.Set (Label_Bwin, L);

         Gtk_New (L, "BW out: ?");
         L.Set_Use_Markup (True);
         Tbl.Attach (L, 0, 1, 1, 2, Xoptions => Shrink);
         Guts.Set (Label_Bwout, L);

         Extra.H_BWin := Agpl.Gdk.Managed.Drawing_Area.Show
           (Attach_In'Access, "lavender", Square => False);

         Extra.H_BWout := Agpl.Gdk.Managed.Drawing_Area.Show
           (Attach_Out'Access, "lavender", Square => False);
      end;

      return Guts;
   end Create;

   ------------
   -- Update --
   ------------

   procedure Update
     (This : in out Stats;
      Guts : in out Agpl.Gdk.Widget_Bundle.Object)
   is
      use Agpl.Conversions;
      use Agpl.Drawing;
      use Agpl.Strings;

      Extra : constant Extra_Access := Extras.Get (Guts.Get_Root);
   begin
      --  PING
      if This.With_Ping then
         Gtk_Label (Guts.Get (Label_Ping)).Set_Label
           ("<tt>Ping: <b>" &
            Rpad (Agpl.Strings.To_String (Integer (This.Roundtrip * 1000.0)), 4) &
            "ms</b></tt>");

         Extra.Ping.Add_Sample (Float (This.Roundtrip));
         Extra.Ping.Trim (Samples);
         Extra.H_Ping.Draw (Figures.Simple_History (0.0, 0.5, Extra.Ping, Samples));
      end if;

      --  BW IN
      Gtk_Label (Guts.Get (Label_BWin)).Set_Label
        ("<tt>BW in : <b>" &
         Rpad (To_Size (This.BW_In, 1), 8) &
         "/s</b></tt>");

      Extra.BWin.Add_Sample (This.BW_in);
      Extra.BWin.Trim (Samples);
      Extra.H_BWin.Draw (Figures.Simple_History (0.0, 0.5, Extra.BWin, Samples));

      --  BW OUT
      Gtk_Label (Guts.Get (Label_BWout)).Set_Label
        ("<tt>BW out: <b>" &
         Rpad (To_Size (This.BW_Out, 1), 8) &
         "/s</b></tt>");

      Extra.BWout.Add_Sample (This.BW_out);
      Extra.BWout.Trim (Samples);
      Extra.H_BWout.Draw (Figures.Simple_History (0.0, 0.5, Extra.BWout, Samples));
   end Update;

end Sancta.Component.Netstats2Gnetstats;
