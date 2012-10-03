with Agpl.Gdk.Custom_Widget;
with Agpl.Gdk.Managed;
with Agpl.Gdk.Palette;
with Agpl.Gtk.User_Data;      pragma Elaborate_All (Agpl.Gtk.User_Data);
with Agpl.Gtk.Widget_Factory; use Agpl.Gtk.Widget_Factory;
with Agpl.Gui;
with Agpl.Strings;

with Glib;
with Glib.Error;
with Glib.Properties;
with Gtk.Button;              use Gtk.Button;
with Gtk.Check_Button;        use Gtk.Check_Button;
--  with Gtk.Drawing_Area;        use Gtk.Drawing_Area;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Handlers;            pragma Elaborate_All (Gtk.Handlers);
with Gtk.Label;               use Gtk.Label;
with Gtk.Notebook;            use Gtk.Notebook;
with Gtk.Table;               use Gtk.Table;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Widget;              use Gtk.Widget;

with Sancta.Ctree.Distributed;
with Sancta.Ctree.CTypes;
with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Component.Ctypes;
with Sancta.Component.Redirect_Listener;
with Sancta.Gui.Config;
with Sancta.Gui.Visor_Data;
with Sancta.Gui.Visor_General_View;
with Sancta.Network.Messages;

with Glib.Object;             use Glib.Object;

package body Sancta.Ctree.Component.Console is

   Name_In_Console : constant String := "name_in_console";

   package SCN renames Sancta.Component.Network;
   package SCRL renames Sancta.Component.Redirect_Listener;
   package SCT renames Sancta.Component.CTypes;
   package SCCT renames Sancta.Ctree.CTypes;
   package SN  renames Sancta.Network;

   type Object_Access is access all Object;

   subtype Drawable is Agpl.Drawing.Drawable'Class;

   package User_Data is new Agpl.Gtk.User_Data (Object_Access);
   package Id_Data   is new Agpl.Gtk.User_Data (Sancta.Node_Id);
   package Name_Data is new Agpl.Gtk.User_Data (String);

   package Check_Button_Handlers is new Standard.Gtk.Handlers.Callback
     (Gtk_Check_Button_Record);

   package Button_Handlers is new Standard.Gtk.Handlers.Callback
     (Gtk_Button_Record);

   procedure Ctree_Console_Go_Clicked (Widget : Gobject_Ptr);
   pragma Export (C, Ctree_Console_Go_Clicked);

   procedure Ctree_Console_Stop_Clicked (Widget : Gobject_Ptr);
   pragma Export (C, Ctree_Console_Stop_Clicked);

   procedure Ctree_Console_Park_Clicked (Widget : Gobject_Ptr);
   pragma Export (C, Ctree_Console_Park_Clicked);

   procedure Ctree_Console_Cancel_Clicked (Widget : Gobject_Ptr);
   pragma Export (C, Ctree_Console_Cancel_Clicked);

   ----------------------
   -- Laser_Switch_ACK --
   ----------------------

   procedure Laser_Switch_ACK (This : Gtk_Check_Button;
                               Key  : External_Key;
                               Val  : Data'Class)
   is
      pragma Unreferenced (Key);
      procedure In_Gtk is
      begin
         This.Set_Sensitive (True);
      end In_Gtk;
   begin
      Agpl.Gdk.Managed.Execute (In_Gtk'Access);
      Log ("ACK for laser: " & SCT.Bool (Val).Value'Img, Debug, Log_Section);
   end Laser_Switch_ACK;

   package ACK is new SCRL.ACKs (Gtk_Check_Button);

   ---------------------------------
   -- Ctree_Console_Laser_Clicked --
   ---------------------------------

   procedure Ctree_Console_Laser_Clicked
     (Button : access Gtk_Check_Button_Record'Class)
   is
      This : constant Object_Access := User_Data.Get_Shared (Button);
      Id   : constant Node_Id := Id_Data.Get (Button);
   begin
      Button.Set_Sensitive (False); -- Wait for ACK
      ACK.Set (Gtk_Check_Button (Button),
               Emits_Laser_Forwarding,
               SCT.Boolean (Button.Get_Active),
               This.Link,
               Id, This.Chan,
               Retry => 0.5,
               CB    => Laser_Switch_ACK'Access);
   exception
      when E : others =>
         Log ("Laser_Clicked: " & Report (E), Error, Log_Section);
   end Ctree_Console_Laser_Clicked;

   ---------------------------------
   -- Ctree_Console_Clear_Clicked --
   ---------------------------------

   procedure Ctree_Console_Clear_Clicked
     (Button : access Gtk_Button_Record'Class)
   is
      This : constant Object_Access := User_Data.Get_Shared (Button);
      Id   : constant Node_Id := Id_Data.Get (Button);
   begin
      This.Local_Canvases.Element (Id).all.Buffer.Clear;
   end Ctree_Console_Clear_Clicked;

   ------------------------------
   -- Ctree_Console_Go_Clicked --
   ------------------------------

   procedure Ctree_Console_Go_Clicked (Widget : Gobject_Ptr) is
      use Ctree.Distributed;
      Button : constant Gtk_Toggle_Button := Proxy (Widget);
      This   : constant Object_Access     := User_Data.Get_Shared (Button);
   begin
      if Button.Get_Active then
         Log ("Releasing robots...", Always, Log_Section);
      end if;
      This.Send (Msg_Operator_Control'(Action => Go));
   end Ctree_Console_Go_Clicked;

   --------------------------------
   -- Ctree_Console_Stop_Clicked --
   --------------------------------

   procedure Ctree_Console_Stop_Clicked (Widget : Gobject_Ptr) is
      use Ctree.Distributed;
      Button : constant Gtk_Toggle_Button := Proxy (Widget);
      This   : constant Object_Access     := User_Data.Get_Shared (Button);
   begin
      if Button.Get_Active then
         Log ("Stopping robots...", Always, Log_Section);
      end if;
      This.Send (Msg_Operator_Control'(Action => Stop));
   end Ctree_Console_Stop_Clicked;

   --------------------------------
   -- Ctree_Console_Park_Clicked --
   --------------------------------

   procedure Ctree_Console_Park_Clicked (Widget : Gobject_Ptr) is
      use Ctree.Distributed;
      Button : constant Gtk_Toggle_Button := Proxy (Widget);
      This   : constant Object_Access     := User_Data.Get_Shared (Button);
   begin
      if Button.Get_Active then
         Log ("Parking robots...", Always, Log_Section);
      end if;
      This.Send (Msg_Operator_Control'(Action => Park, Towards => 3.14159));
   end Ctree_Console_Park_Clicked;

   ----------------------------------
   -- Ctree_Console_Cancel_Clicked --
   ----------------------------------

   procedure Ctree_Console_Cancel_Clicked (Widget : Gobject_Ptr) is
      use Ctree.Distributed;
      Button : constant Gtk_Button := Proxy (Widget);
      This   : constant Object_Access     := User_Data.Get_Shared (Button);
   begin
      Log ("Cancelling mission...", Always, Log_Section);

      This.Send (Msg_Operator_Control'(Action => Cancel));
   end Ctree_Console_Cancel_Clicked;

   ---------------
   -- New_Label --
   ---------------

   function New_Label (S : String) return Gtk_Label is
      L : Gtk_Label;
   begin
      Gtk_New (L, S);
      return L;
   end New_Label;

   ----------
   -- Send --
   ----------

   procedure Send (This : Object;
                   Msg  : Sancta.Network.Message'Class)
   is
      package SN renames Sancta.Network;
   begin
      This.Link.Send (SN.New_Address (This.Chan), Msg);
   end Send;

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
     (Config : Comp_Config)
      return Sancta.Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);

      procedure Get_Nodes (I : SN.Node_Sets.Cursor) is
      begin
         This.Nodes.Append (Image (SN.Node_Sets.Element (I)));
      end Get_Nodes;

      ------------
      -- Create --
      ------------

      procedure Create is
         X : Gtkada_Builder;
         E : Glib.Error.GError;
         use type Glib.Gint;
      begin
         Gtk_New (X);
         E := X.Add_From_File (This.Option (Opt_Glade_Xml, Def_Glade_Xml));
         if Glib.Error.Get_Code (E) /= 0 then
            raise Program_Error with "Gtkbuilder error" &
              Glib.Error.Get_Code (E)'Img;
         end if;

         Agpl.Gdk.Managed.GtkBuilder_Connect_Void (X);
         User_Data.Set_Shared (X.Get_Widget ("console"), This);
         This.Gui := X;

         declare
            Opts : Gui.Config.Object;
         begin
            Opts.Laser_Scans := 1;
            Opts.Laser_Range := 81.0;
            Opts.Draw_Grid   := False;
            Opts.Show_Poses  := False;
            This.Visor := new Gui.Visor.Object (This.Link);
            This.Visor.Create_Bare (Opts);

            Sancta.Gui.Visor_General_View.Register;

            This.General_View := This.Visor.Create_Widget
              (null, Gui.Visor_General_View.View_Name);
            Gtk_Notebook (This.Gui.Get_Widget ("maptabs")).
              Append_Page (This.General_View.Area, New_Label ("Grid"));
            This.General_View.Area.Show_All;
         end;
      end Create;

      -----------------------
      -- Build_Dynamic_Gui --
      -----------------------

      procedure Build_Dynamic_Gui is
         HB : constant Gtk_HBox := Gtk_HBox (This.Gui.Get_Widget ("box_nodes"));
      begin
         for I in This.Nodes.First_Index .. This.Nodes.Last_Index loop
            declare
               Id : constant Node_Id := +This.Nodes.Element (I);
               C  : constant Canvas_Access := new Robot_Canvas;
               VB : Gtk_VBox;
               VBHB : Gtk_HBox;
               --  Below the VBox for each robot there's a HBox with the
               --  clear button and laser checkbox.

               F  : Gtk_Frame;

               Clear       : Gtk_Button;
               Laser_Check : Gtk_Check_Button;

               procedure Add (Widget : Gtk_Widget) is
               begin
                  Widget.Set_Size_Request (8, 8); -- Por poner algo
                  VB.Pack_Start (Widget, Padding => 2);
               end Add;
            begin
               --  Vertical container per robot
               This.Local_Canvases.Insert (Id, C);
               Gtk_New (F, This.Nodes.Element (I));
--                 F.Set_Size_Request (100);
               Gtk_New_VBox (VB, Homogeneous => False, Spacing => 1);
               F.Add (VB);
               This.Local_Vboxes.Insert (Id, VB);
               HB.Pack_Start (F, Padding => 2);

               --  Horizontal top controls container per robot
               Gtk_New_HBox (VBHB);
               VBHB.Set_Homogeneous (False);
               VB.Pack_Start (VBHB, Expand => False);

               --  Clear button
               Gtk_New (Clear, "Clear");
               Id_Data.Set (Clear, Id);
               VBHB.Pack_Start (Clear, Expand => False);
               Button_Handlers.Connect
                 (Clear,
                  "clicked",
                  Ctree_Console_Clear_Clicked'Access);

               --  Laser toggle
               Gtk_New (Laser_Check, "Laser");
               Laser_Check.Set_Active (False);
               Id_Data.Set (Laser_Check, Id);
               VBHB.Pack_Start (Laser_Check, Expand => False);
               Check_Button_Handlers.Connect
                 (Laser_Check,
                  "toggled",
                  Ctree_Console_Laser_Clicked'Access);

               --  Drawing area per robot
               This.Local_DAreas.Insert
                 (Id, Agpl.Gdk.Managed.Drawing_Area.Show (Add'Access));

               --  Link histories
               for J in This.Nodes.First_Index .. This.Nodes.Last_Index loop
                  if I /= J then
                     declare
                        LH : constant Gtk.Link_History.Object_Access :=
                               new Gtk.Link_History.Object (Samples => 300);
                        LHW : Gtk_Widget;
                     begin
                        if I + 1 = J then
                           LH.Create (This.Q_Threshold,
                                      +This.Nodes.Element (I),
                                      +This.Nodes.Element (J),
                                      LHW);
                        else
                           LH.Create (This.Q_Threshold,
                                      +This.Nodes.Element (I),
                                      +This.Nodes.Element (J),
                                      LHW,
                                      "gainsboro");
                        end if;
                        VB.Pack_Start (LHW, Expand  => False);
                        This.Link_Histories.Insert ((I, J), LH);
                     end;
                  end if;
               end loop;
            end;
         end loop;

         HB.Show_All;
      end Build_Dynamic_Gui;

   begin
      This.Period.Set_Period
        (Duration'Value (This.Option (Opt_Period, Def_Period'Img)));

      This.Chan := SN.Value (This.Option (Opt_Channel));

      This.Q_Threshold :=
        Sancta.Network.Qualities.Quality'Value
          (This.Option (Opt_Quality_Threshold));

      This.Link := SCN.Network (This.Input (Requires_Link)).Link;
      This.Link.Node_Names.Iterate (Get_Nodes'Access);
      This.Link.Subscribe (This.Inbox'Access, This.Chan);

      Agpl.Gdk.Managed.Execute (Create'Access);
      Agpl.Gdk.Managed.Execute (Build_Dynamic_Gui'Access);

      This.Subscribe (Requires_Full_Signal);
      This.Subscribe (Requires_Local_Parcels);
      This.Subscribe (Requires_Global_Parcels);
      This.Subscribe (Requires_Tabs);

      This.Map_Drawer := Gui.Visor_Data.Drawer (This.Visor.Get_Data);
      This.Output
        (Provides_Map_Drawer,
         SCT.Drawer'(Drawer => This.Map_Drawer));

      return Sancta.Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
      M : Agpl.Monitor.Object (This.Mutex'Access); pragma Unreferenced (M);

      use Agpl.Gdk.Palette;
      use Agpl.Strings;

      -------------------
      -- Update_Signal --
      -------------------

      procedure Update_Signal (Signal : SCCT.Full_Signal) is

         Bots   : constant Sancta.Network.Qualities.Node_Vector :=
                          Signal.Links.Nodes;

         procedure Update_Signal is
            T : constant Gtk_Table :=
                  Gtk_Table (This.Gui.Get_Widget ("instant_signal"));
            use Glib;
            use Glib.Properties;

            --------------------
            -- Recreate_Table --
            --------------------

            procedure Recreate_Table is
               N_Bots : constant Natural := Natural (Bots.Length);
            begin
               T.Resize (Guint (N_Bots + 1), Guint (N_Bots + 1));

               --  Don't know what happens to existing children... leak here?
               --  There's no detach, so...
               for R in 0 .. N_Bots loop
                  for C in 0 .. N_Bots loop
                     if not This.Instant_Signal_Labels.Contains ((R, C)) then
                        declare
                           Label : Gtk_Label;
                        begin
                           Gtk_New (Label);
                           Label.Set_Single_Line_Mode (True);
                           Label.Set_Use_Markup (True);
                           Label.Set_Selectable (False);
                           This.Instant_Signal_Labels.Insert ((R, C), Label);
                           T.Attach_Defaults (Label,
                                              Guint (C),
                                              Guint (C + 1),
                                              Guint (R),
                                              Guint (R + 1));
                        end;
                     end if;
                  end loop;
               end loop;

               for I in Bots.First_Index .. Bots.Last_Index loop
                  This.Instant_Signal_Labels.Element ((0, I)).Set_Label
                    ("<b>" & Bots.Element (I) & "</b>");
                  This.Instant_Signal_Labels.Element ((I, 0)).Set_Label
                    ("<b>" & Bots.Element (I) & "</b>");
               end loop;

               T.Show_All;

            end Recreate_Table;

         begin
            for I in This.Nodes.First_Index .. This.Nodes.Last_Index loop
               for J in This. Nodes.First_Index .. This.Nodes.Last_Index loop
                  if I /= J then
                     This.Link_Histories.Element ((I, J)).all.Update (Signal);
                  end if;
               end loop;
            end loop;

            if Integer (Get_Property (T, N_Rows_Property)) /=
              Integer (Bots.Length) + 1
            then
               Recreate_Table;
            end if;

            for R in Bots.First_Index .. Bots.Last_Index loop
               Log ("RECEIVED FULL SIGNAL bot " & Bots.first_element, Never);
               for C in Bots.First_Index .. Bots.Last_Index loop
                  declare
                     use type RGB_Component;
                     subtype QF is Float range
                       Float (Sancta.Network.Qualities.Quality'First) ..
                       Float (Sancta.Network.Qualities.Quality'Last);
                     subtype QFgood is QF range QF (This.Q_Threshold) .. QF'Last;
                     subtype QFbad is QF range QF'First .. QF (This.Q_Threshold);

                     function Ggood is new Agpl.Gdk.Palette.Gradient
                       (QFgood, RGB_Component'Last, RGB_Component'First);
                     function Gbad is new Agpl.Gdk.Palette.Gradient
                       (QFbad, RGB_Component'First, RGB_Component'Last);

                     Q : constant QF := QF
                       (Signal.Links.Get (+Bots.Element (R), +Bots.Element (C)));

                     B  : Ustring;
                     BB : Ustring;
                  begin
                     if R + 1 = C then
                        B  := +"<b>";
                        BB := +"</b>";
                     end if;

                     if Q < Float (This.Q_Threshold) then
                        This.Instant_Signal_Labels.Element ((R, C)).Set_Label
                          ("<span background=""" &
                           Get_Color_Name
                             (RGB_Component'Last,
                              Gbad (Q),
                              Gbad (Q)) &
                           """><tt><i>" &
                           (+B) &
                           Rpad (To_String (Q, 1), 5) &
                           (+BB) &
                           "</i></tt></span>");
                     else
                        This.Instant_Signal_Labels.Element ((R, C)).Set_Label
                          ("<span background=""" &
                           Get_Color_Name
                             (Ggood (Q),
                              RGB_Component'Last,
                              Ggood (Q)) &
                           """><tt>" &
                           (+B) &
                           Rpad (To_String (Q, 1), 5) &
                           (+BB) &
                           "</tt></span>");
                     end if;
                  end;
               end loop;
            end loop;
         end Update_Signal;
      begin
         Log ("RECEIVED FULL SIGNAL", Never);
         Agpl.Gdk.Managed.Execute (Update_Signal'Access);
      end Update_Signal;

      ---------------------------
      -- Process_Custom_Widget --
      ---------------------------

      procedure Process_Custom_Widget
        (Id   :        Node_Id;
         Name :        String;
         W    : in out Agpl.Gdk.Custom_Widget.Remote'Class)
      is
         --  There's some excess copying going on here that could be
         --  optimized away.

         Key    : constant Node_Widget_Key := (Id, +Name);
         I      : constant Node_Widget_Maps.Cursor := This.Local_Widgets.Find (Key);
         Bundle : Agpl.Gdk.Widget_Bundle.Object;

         procedure Reorder_Children (V : Gtk_VBox) is
            use Glib;
            First : Natural;
            Last  : Natural;
         begin
            for I in 0 .. Gint'Last loop
               begin
                  if Name_Data.Get (V.Get_Child (I), Name_In_Console) /= "" then
                     First := Natural (I);
                     exit;
                  end if;
               exception
                  when others =>
                     Log ("WIDGET" & I'Img & " HAS NO NAME", Never);
               end;
            end loop;

            for I in First .. Natural'Last loop
               exit when V.Get_Child (Gint (I)) = null;
               Last := I;
               Log ("WIDGET" & I'Img & " IS NOT NULL", Never);
            end loop;

            --  Bubble shitsort
            for I in First .. Last - 1 loop
               for J in I + 1 .. Last loop
                  if Name_Data.Get (V.Get_Child (Gint (I)), Name_In_Console) >
                     Name_Data.Get (V.Get_Child (Gint (J)), Name_In_Console)
                  then
                     Log ("MOVING CHILD" & J'Img & " TO" & I'Img, Never);
                     V.Reorder_Child (V.Get_Child (Gint (J)), Gint (I));
                     Log ("MOVED CHILD " & J'Img & " TO" & I'Img, Never);
                  end if;
               end loop;
            end loop;
         end Reorder_Children;

         procedure Create is
         begin
            Bundle := W.Create;
            Bundle.Get_Root.Show_All;
            Name_Data.Set (Bundle.Get_Root, Name, Name_In_Console);
            This.Local_Vboxes.Element (Id).Pack_Start
              (Bundle.Get_Root, Expand => False);

            Reorder_Children (This.Local_Vboxes.Element (Id));
         end Create;

         procedure Update is
         begin
            W.Update (Bundle);
         end Update;
      begin
         if Node_Widget_Maps.Has_Element (I) then
--              Log ("Updating custom widget: " & External_Tag (W'Tag),
--                   Debug, Log_Section);
            Bundle := Node_Widget_Maps.Element (I);
         else
            Log ("Creating custom widget: " & External_Tag (W'Tag),
                 Debug, Log_Section);
            begin
               Agpl.Gdk.Managed.Execute (Create'Access);
            exception
               when Program_Error =>
                  Log ("Creation failed for widget " & External_Tag (W'Tag),
                       Error, Log_Section);
            end;
         end if;

         Agpl.Gdk.Managed.Execute (Update'Access);

         This.Local_Widgets.Include (Key, Bundle);
      end Process_Custom_Widget;

   begin
      if Key = Requires_Full_Signal then
         if This.Signal_Timer.Elapsed >= This.Period.Get_Period then
            This.Signal_Timer.Reset;
            Update_Signal (SCCT.Full_Signal (Value));
         end if;
      elsif Key = Requires_Global_Parcels then
         declare
            Parcel : Data_Parcel'Class renames Data_Parcel'Class (Value);
         begin
            if Parcel.Datum.Ref.all in Drawable'Class then
               This.Map_Drawer.Draw
                 (+Parcel.Label, Drawable'Class (Parcel.Datum.Ref.all));
            end if;
         end;
      elsif Key = Requires_Local_Parcels then
         declare
            Parcel : Data_Parcel'Class renames Data_Parcel'Class (Value);
         begin
            if Parcel.Datum.Ref.all in Drawable'Class then
               This.Local_Canvases.Element (Parcel.Owner).all.Buffer.Draw
                 (+Parcel.Label, Drawable'Class (Parcel.Datum.Ref.all));
            end if;

            if Parcel.Datum.Ref.all in Agpl.Gdk.Custom_Widget.Remote'Class then
               Process_Custom_Widget
                 (Parcel.Owner,
                  +Parcel.Label,
                  Agpl.Gdk.Custom_Widget.Remote'Class (Parcel.Datum.Ref.all));
            end if;
         end;
      elsif Key = Requires_Tabs then
         declare
            Parcel : Data_Parcel'Class renames Data_Parcel'Class (Value);
            Handle : Agpl.Gdk.Managed.Drawing_Area.Handle;
            procedure Attach (W : Gtk_Widget) is
            begin
               Gtk_Notebook (This.Gui.Get_Widget ("maptabs")).
                 Append_Page (W, New_Label (+Parcel.Label));
               W.Show_All;
            end Attach;
         begin
            if not This.Tabs.Contains (+Parcel.Label) then
               Handle := Agpl.Gdk.Managed.Drawing_Area.Show (Attach'Access);
               This.Tabs.Insert (+Parcel.Label, Handle);

               if Parcel.Datum.Ref.all in Agpl.Gui.Event_Handler'Class then
                  Handle.Attach
                    (Agpl.Gui.Event_Handler'Class (Parcel.Datum.Ref.all));
               end if;
            else
               Handle := This.Tabs.Element (+Parcel.Label);
            end if;

            Handle.Draw (Agpl.Drawing.Drawable'Class (Parcel.Datum.Ref.all));
         end;
      end if;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      M : Agpl.Monitor.Object (This.Mutex'Access); pragma Unreferenced (M);

      --------------------
      -- Flush_Canvases --
      --------------------

      procedure Flush_Canvases (I : Id_Canvas_Maps.Cursor) is
         use Id_Canvas_Maps;

         C : constant Canvas_Access := Element (I);
         H : Agpl.Gdk.Managed.Drawing_Area.Handle :=
               This.Local_DAreas.Element (Key (I));
      begin
         H.Draw (C.Buffer);
      end Flush_Canvases;

      ----------
      -- Read --
      ----------

      procedure Read (Msg  : SN.Message'Class;
                      Meta : SN.Message_Metadata)
      is
         Id : constant Node_Id := Meta.Sender;
         subtype Drawable is Agpl.Drawing.Drawable;
      begin
         if Msg in SN.Messages.Redirect_Type'Class then
            declare
               R : SN.Messages.Redirect_Type'Class renames
                 SN.Messages.Redirect_Type'Class (Msg);
            begin
               if R.Val.Ref.all in Drawable'Class then
                  This.Local_Canvases.Element (Id).all.Buffer.Draw
                    (R.Key.Ref.all, Drawable'Class (R.Val.Ref.all));
               end if;
            end;
         elsif Msg in Drawable'Class then
            This.Local_Canvases.Element (Id).all.Buffer.Draw
              ((-Id) & External_Tag (Msg'Tag), Drawable'Class (Msg));
         end if;
      end Read;

   begin
      while not This.Inbox.Is_Empty loop
         This.Inbox.Open (Read'Access, Block => False);
      end loop;

      This.Local_Canvases.Iterate (Flush_Canvases'Access);

      This.Visor.Run;
      This.General_View.Area.Queue_Draw;

      This.Period.Next (Next);
   end Run;

   function "<" (L, R : Node_Widget_Key) return Boolean is
      use ASU;
   begin
      return
        L.Id < R.Id or else
       (L.Id = R.Id and then L.Name < R.Name);
   end "<";

end Sancta.Ctree.Component.Console;
