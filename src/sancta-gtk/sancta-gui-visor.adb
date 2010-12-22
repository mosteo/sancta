with Sancta.Component.Types,
     Sancta.Datastore;
with Sancta.Gui.Visor_Factory,
     Sancta.Gui.Visor_General_View,
     Sancta.Gui.Visor_Mission,
     Sancta.Starter;
with Sancta.Types;

pragma Warnings (Off);
with Sancta.Tasks.Include; -- Force inclusion of tagged types used.
pragma Warnings (On);

with Agpl.Calendar.Format;
with Agpl.Gdk.Managed;
with Agpl.Gdk.Snapshot;
with Sancta.Tasks.Handle;
with Sancta.Tasks.Containers;
with Agpl.Strings;
with Agpl; use Agpl;

with Gtk.Box;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Handlers; pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu_Bar;
with Gtk.Paned;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;

with Ada.Tags;
--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Gui.Visor is

   type Visor_Plus_View is record
      Visor_Access : Visor.Object_Access;
      View_Access  : Visor_Widget.Object_Access;
   end record;

   function Delete_View (Widget : access Gtk_Widget_Record'Class;
                         Event  :        Gdk.Event.Gdk_Event_Expose;
                         Vv     :        Visor_Plus_View)
                         return          Boolean;

   package Visor_Handlers_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Visor.Object_Access);

   package VV_Handlers_UR is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record,
      Boolean,
      Visor_Plus_View);

   package Visor_Handlers_U is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record,
      Visor.Object_Access);

   -------------------
   -- Create_Common --
   -------------------

   procedure Create_Common (This : in out Object;
                            Opts :        Config.Object)
   is
   begin
      This.Config := Opts;
      This.Data.Set_Config (Opts);

      begin
         This.Data.Get_Mission.all.Parse_File (+Opts.Mission_File);
      exception
         when E : others =>
            Log ("Parsing mission failed: " & Report (E), Error);
      end;
      This.Data.Get_Mission.Print_Summary;

      Visor_Data.Set_Link (This.Data,
                           This.Link.all'Unchecked_Access);

      This.Subscribe (Gui_Channel);

      This.Laser_Scans := This.Config.Laser_Scans;
   end Create_Common;

   ---------------------
   -- Create_Menu_Ver --
   ---------------------

   procedure Create_Menu_Ver (This : in out Object;
                              Menu :        Gtk_Menu_Item)
   is
      package Cbu  renames Visor_Handlers_U;
      procedure Internal is
      begin
         --  Create the View menu with the registered creators
         Gtk_New (This.View_Menu);
         declare
            use Visor_Factory.Creator_Maps;
            -- Add_Entry --
            procedure Add_Entry (I : in Cursor) is
               Item : constant Gtk_Menu_Item_With_View :=
                        new Gtk_Menu_Item_With_View_Record'
                          (Gtk_Menu_Item_Record with Name => +Key (I));
            begin
               --  Gtk_New (Item, Key (I));
               Initialize (Gtk_Menu_Item (Item), Key (I));
               Append (This.View_Menu, Item);
               Show (Item);
               --  And connect to the creator procedure:
               Cbu.Connect
                 (Item,
                  "activate",
                  Cbu.To_Marshaller
                    (View_Clicked'Access),
                  This'Unchecked_Access);
            end Add_Entry;
         begin
            Visor_Factory.Creators.Iterate (Add_Entry'Access);

            Set_Submenu (Menu, This.View_Menu);
         end;
      end Internal;
   begin
      Agpl.Gdk.Managed.Execute (Internal'Access);
   end Create_Menu_Ver;

   ------------
   -- Create --
   ------------

   procedure Create (This : in out Object;
                     Opts :        Sancta.Gui.Config.Object) is
      package Cbur renames Visor_Handlers_UR;
      package Cbu  renames Visor_Handlers_U;
      procedure Internal is
      begin
         This.Create_Common (Opts);

         Top_Visor_Pkg.Gtk_New (This.Top);
         Top_Visor_Pkg.Show_All (This.Top);

         Cbur.Connect
           (This.Top,
            "delete_event",
            Cbur.To_Marshaller
              (Delete'Access),
            This'Unchecked_Access);

         Cbu.Connect
           (This.Top.Clear_Button,
            "clicked",
            Cbu.To_Marshaller
              (Clear_Clicked'Access),
            This'Unchecked_Access);

         Cbu.Connect
           (This.Top.Stop_Button,
            "clicked",
            Cbu.To_Marshaller
              (Stop_Clicked'Access),
            This'Unchecked_Access);
      end Internal;

   begin
      Agpl.Gdk.Managed.Execute (Internal'Access);
      This.Create_Menu_Ver (This.Top.Ver);
   end Create;

   -------------------
   -- Create_Simple --
   -------------------

   procedure Create_Simple (This : in out Object;
                            Opts :        Config.Object)
   is
      package Cbur renames Visor_Handlers_UR;

      procedure Internal is
         Win : Gtk.Window.Gtk_Window;
         Box : Gtk.Box.Gtk_Vbox;
      begin
         --  Top window
         Gtk.Window.Gtk_New (Win, Window_Toplevel);
         Win.Unset_Focus_Chain;
         Win.Set_Position (Win_Pos_None);
         Win.Set_Modal (False);
         Win.Set_Resizable (True);
         Cbur.Connect
           (Win,
            "delete_event",
            Cbur.To_Marshaller
              (Delete'Access),
            This'Unchecked_Access);

         --  Box
         Gtk.Box.Gtk_New_Vbox (Box);
         Box.Unset_Focus_Chain;
         Win.Add (Box);

         --  Pane
         declare
            Pan   : Gtk.Paned.Gtk_Vpaned;
            W1    : constant Widget_Container :=
                      This.Create_Widget (Win, Visor_General_View.View_Name);
            W2    : constant Widget_Container :=
                      This.Create_Widget (Win, Visor_Mission.View_Name);
            Chain : Gtk.Widget.Widget_List.Glist;
         begin
            Gtk.Paned.Gtk_New_Vpaned (Pan);
            Pan.Unset_Focus_Chain;
            Widget_List.Append (Chain, W1.Widget.Get_Widget);
            Pan.Set_Focus_Chain (Chain);
            Pan.Pack1 (W1.Widget.Get_Widget, True, True);
            Pan.Pack2 (W2.Widget.Get_Widget, True, True);
            if Opts.Show_Mission then
               Pan.Set_Position (400);
            else
               Pan.Set_Position (9999);
            end if;
            Box.Pack_End  (Pan);
            W1.Widget.Get_Widget.Grab_Focus;
            This.Widgets.Append (W1);
            This.Widgets.Append (W2);

            --  Focus on top pane:
            declare
               Chain : Gtk.Widget.Widget_List.Glist;
            begin
               Widget_List.Append (Chain, Gtk_Widget (Pan));
               Win.Set_Focus_Chain (Chain);
            end;
         end;

         --  Menu
         declare
            Menu : Gtk.Menu_Bar.Gtk_Menu_Bar;
            Ver  : Gtk.Menu_Item.Gtk_Menu_Item;
         begin
            Gtk.Menu_Bar.Gtk_New (Menu);
            Gtk.Menu_Item.Gtk_New_With_Mnemonic (Ver, "_Ver");
            This.Create_Menu_Ver (Ver);
            Menu.Append (Ver);
            Box.Pack_Start (Menu, False, False);
         end;

         Win.Set_Default_Size (800, 600);
         Win.Show_All;
      end Internal;
   begin
      This.Create_Common (Opts);
      Agpl.Gdk.Managed.Execute (Internal'Access);
   end Create_Simple;

   -----------------
   -- Create_Bare --
   -----------------

   procedure Create_Bare (This : in out Object;
                          Opts : Config.Object)
   is
   begin
      This.Create_Common (Opts);
   end Create_Bare;

   -------------------
   -- Create_Widget --
   -------------------

   function Create_Widget (This   : Object;
                           Window : Gtk_Window;
                           Kind   : String)
                           return Widget_Container
   is
      X : constant Visor_Widget.Object_Access :=
            Visor_Factory.Creators.Element (Kind)
            (Visor_Factory.Creation_Context'(Options => This.Config));
   begin
      X.Set_Data (This.Data'Unrestricted_Access);
      return (Window => Window,
              Area   => X.Get_Widget,
              Widget => X);
   end Create_Widget;

   -----------------
   -- Create_View --
   -----------------

   procedure Create_View (This : in out Object; Kind : in String)
   is
      package Vvur renames Vv_Handlers_Ur;
      use Visor_Factory.Creator_Maps;
      use type Visor_Widget.Object_Access;

      X : Widget_Container;
      C : constant Cursor := Find (Visor_Factory.Creators, Kind);
   begin
      Gtk_New (X.Window);
      Initialize (X.Window, Window_Toplevel);
      Set_Position (X.Window, Win_Pos_None);
      Set_Modal (X.Window, False);
      Set_Title (X.Window, Kind);

      --  Call to Create
      X.Widget :=
        Element (C) (Visor_Factory.Creation_Context'(Options => This.Config));

      if X.Widget /= null then

         Log ("New widget created", Informative);

         X.Widget.Set_Data (This.Data'Unchecked_Access);

         Add (X.Window, Visor_Widget.Get_Widget (X.Widget.all));

         This.Widgets.Append (X);

         --  Connect with destructor:
         Vvur.Connect
           (X.Window,
            "delete_event",
            Vvur.To_Marshaller
              (Delete_View'Access),
            Visor_Plus_View'(Visor_Access => This'Unchecked_Access,
                             View_Access  => X.Widget));

         Show_All (X.Window);

      else
         Log ("Visor.Create_View: Unable to create!",
              Error);
      end if;
   exception
      when E : others =>
         Log ("Sancta.Gui.Visor.Create_View: " & Report (E), Error);
   end Create_View;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This : in Object) is
   begin
      This.Link.Send (Network.New_Address (Emergency_Channel),
                      Network.Messages.Clear_Tasks);
   end Clear_Tasks;

   --------------------
   -- Emergency_Stop --
   --------------------

   procedure Emergency_Stop (This : in Object) is
   begin
      This.Link.Send (Network.New_Address (Emergency_Channel),
                      Network.Messages.Shutdown);
   end Emergency_Stop;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (This : in Object) return Visor_Data.Object_Access is
   begin
      return This.Data'Unrestricted_Access;
   end Get_Data;

   -------------
   -- Is_Done --
   -------------

   function Is_Done (This : in Object) return Boolean is
   begin
      return This.Shutdown;
   end Is_Done;

   ---------------
   -- Local_Log --
   ---------------

   procedure Local_Log (This    : in out Object;
                        Text    : in     String;
                        Level   : in     Agpl.Trace.Levels;
                        Section : in     String;
                        Sender  : in     String)
   is
   begin
      --  Update our store of trace messages
      declare
         I     : Gtk_Tree_Iter;
         Store : constant Gtk_Tree_Store := Visor_Data.Get_Logs (This.Data);
      begin
         Append (Store, I, Null_Iter);
         Set (Store, I, Visor_Data.Sender_Column,  Sender);
         Set (Store, I, Visor_Data.Date_Column,    Agpl.Calendar.Format.Timestamp);
         Set (Store, I, Visor_Data.Level_Column,   Level'Img);
         Set (Store, I, Visor_Data.Section_Column, Section);
         Set (Store, I, Visor_Data.Text_Column,    Text);
      end;
   end Local_Log;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
      Aux_Data : constant Visor_Data.Object_Access := This.Data'Unchecked_Access;
   begin
      if M in Robot_Data.Network_Update then
         This.Process_Msg (Meta.Sender, Robot_Data.Network_Update (M));
      elsif M in Network.Messages.Laser_Type then
         This.Process_Msg (Meta.Sender, Network.Messages.Laser_Type (M));
      elsif M in Network.Messages.Task_Done_Type then
         --  Mark some task as finished
         Log ("Received report: Task" & Network.Messages.Task_Done_Type (M).Id'Img &
              " finished.", Debug, Log_Section);
         Aux_Data.Get_Mission.all.Mark_Finished
           (Network.Messages.Task_Done_Type (M).Id);
      elsif M in Network.Messages.Redirect_Type then
         This.Process_Msg (Meta.Sender, Network.Messages.Redirect_Type (M));
      elsif M in Network.Messages.Hello_Type then
        null; --  Safely discardable messages
      else
         Log ("Unknown Message: " & Ada.Tags.External_Tag (M'Tag),
              Agpl.Trace.Debug);
      end if;
   end Process_Incoming_Packet;

   ----------------
   -- Agent_Kind --
   ----------------

   function Agent_Kind (This : Object;
                        Name : String) return Types.Agent_Kinds
   is
      pragma Unreferenced (This);
   begin
      if Name = "zak" then
         return Types.Gui;
      else
         return Types.Robot;
      end if;
--        return Types.Agent_Kinds'Value
--          (Xml.Get_Attribute (This.Config.Named_Options (Name), "kind", "robot"));
   end Agent_Kind;

   -----------------
   -- Process_Msg --
   -----------------

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Robot_Data.Network_Update)
   is
      use Robot_Data;
      use type Types.Agent_Kinds;

      Sender : constant String := Image (Src);
   begin
      if Agent_Kind (This, Sender) /= Types.Robot then
         return; -- <------ EARLY EXIT
      end if;

      declare
         Bot    : constant Visor_Data.Robot_Access :=
                    Visor_Data.Locate_Robot (This.Data'Access, Sender);
      begin
         Bot.Last_Seen.Reset;

         case Msg.Kind is
            when None => null;
            when Pose =>
               --            Log ("Acquired bot " & (+Bot.Name), Always);
               Bot.Agent.Set_Pose (Msg.Position);
               --  Bot.Agent.Set_Vel (Msg.Velocity);
            when All_Tasks =>
               Bot.Agent.Set_Tasks (Msg.Tasks);
               Bot.Cost := Msg.Cost;
            when Current_Task =>
               declare
                  use Sancta.Tasks.Handle;
                  use Sancta.Tasks.Containers.Lists;
                  Tasks : List := Bot.Agent.Get_Tasks;
                  I     : constant Cursor := Find (Tasks, Get (Msg.Job));
               begin
                  if Has_Element (I) then
                     Replace_Element (Tasks, I, Get (Msg.Job));
                     Bot.Agent.Set_Tasks (Tasks);
                  else
                     Log ("Visor.Process_Msg: Task not found when Current_Task received",
                          Warning);
                  end if;
               end;
            when Status =>
               Bot.Status := Msg.Info;
            when Robot_Data.Trace =>
               --  Update our store of trace messages
               Local_Log (This,
                          Text    => +Msg.Text,
                          Level   =>  Msg.Level,
                          Section => +Msg.Section,
                          Sender  =>  Sender);
         end case;
      end;

      This.Update_All (Src, Msg);
   end Process_Msg;

   -----------------
   -- Process_Msg --
   -----------------

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Network.Messages.Laser_Type)
   is
      use Robot_Data;
      use type Types.Agent_Kinds;

      Sender : constant String := Image (Src);
   begin
      if Agent_Kind (This, Sender) /= Types.Robot then
         return; -- <------ EARLY EXIT
      end if;

      declare
         Bot    : constant Visor_Data.Robot_Access :=
                    Visor_Data.Locate_Robot (This.Data'Access, Sender);
      begin
         Log ("Laser received", Never);
         Bot.Laser_Scans.Append (Msg);
         while Natural (Bot.Laser_Scans.Length) > This.Laser_Scans loop
            Bot.Laser_Scans.Delete_First;
         end loop;
      end;
   end Process_Msg;

   -----------------
   -- Process_Msg --
   -----------------

   procedure Process_Msg (This : in out Object;
                          Src  : in     Node_Id;
                          Msg  : in     Network.Messages.Redirect_Type)
   is
      use Robot_Data;
      use type Types.Agent_Kinds;

      Sender : constant String := Image (Src);

      type Recognized_Keys is
        (Gui_Pose,
         Gui_Laser,
         Gui_Tasks);
   begin
      if Agent_Kind (This, Sender) /= Types.Robot then
         return; -- <------ EARLY EXIT
      end if;

      declare
         Bot    : constant Visor_Data.Robot_Access :=
                    Visor_Data.Locate_Robot (This.Data'Access, Sender);
      begin
         Bot.Last_Seen.Reset;

         begin
            declare
               Test : constant Recognized_Keys :=
                        Recognized_Keys'Value (Msg.Key.Get);
            begin
               if Test'Valid then
                  null; -- Absurd
               end if;
            end;

            case Recognized_Keys'Value (Msg.Key.Get) is
               when Gui_Pose =>
                  Bot.Agent.Set_Pose (Datastore.Pose (Msg.Val.Get).Pose);
                  This.Update_All (Src,
                    (Network.Message with Pose,
                     Position => Datastore.Pose (Msg.Val.Get).Pose,
                     Velocity => (0.0, 0.0, 0.0)));
               when Gui_Tasks =>
                  Bot.Agent.Set_Tasks
                    (Component.Types.Task_List (Msg.Val.Ref.all).Tasks);
--                  Bot.Cost := Msg.Cost;
               when others =>
                  null;
            end case;
         exception
            when others =>
               null;
--                 Log ("Received unrecognized key: " & Msg.Key.Get, Warning);
         end;
      end;
   end Process_Msg;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object) is
   begin
      --  Listen for network messages
      select
         delay 0.5;
         Log ("Visor.Run: Netlistener.Run too long", Warning);
      then abort
         Netlistener.Run (Netlistener.Object (This));
      end select;

      --  Run auctions
      This.Data.Run_Auctions;

      --  Remove ancient laser readings
      if This.Laser_Cron.Elapsed >= 0.5 then
         This.Laser_Cron.Reset;
         declare
            Bots : constant Visor_Data.Robot_Array := This.Data.Get_Robots;
         begin
            for I in Bots'Range loop
               while Natural (Bots (I).Laser_Scans.Length) > This.Laser_Scans loop
                  Bots (I).Laser_Scans.Delete_First;
               end loop;
            end loop;
         end;
      end if;

      --  Local timed updates
      if This.Update_Cron.Elapsed >= 1.0 then
         This.Update_Cron.Reset;
         This.Update_All (This.Link.Id,
                          (Network.Message with Robot_Data.None));
      end if;

      --  Snapshots
      if This.Recording_Next <= Clock then
         This.Recording_Next := This.Recording_Next + 0.5;
         if This.Is_Recording or else This.Data.Is_Recording then
            This.Capture_Views;
            This.Recording_Index := This.Recording_Index + 1;
         end if;
      end if;
   end Run;

   --------------
   -- Set_Done --
   --------------

   procedure Set_Done (This : in out Object) is
   begin
      This.Shutdown := True;
   end Set_Done;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All (This : in out Object;
                         Src  : in     Node_Id;
                         Msg  : in     Robot_Data.Network_Update)
   is
      use Widget_Lists;
      procedure Do_Update (I : in Cursor) is
      begin
         Element (I).Widget.Update (Src, Msg);
      end Do_Update;
   begin
      This.Widgets.Iterate (Do_Update'Access);
   end Update_All;

   ----------------------------------------------

   ------------
   -- Delete --
   ------------

   function Delete (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    Visor  :        Gui.Visor.Object_Access)
                    return          Boolean
   is
      pragma Unreferenced (Widget, Event);
   begin
      Log ("Closing visor...", Always);
      --  Hide (Visor.Top);
      Visor.Set_Done;

      --  To hell with cleaning out
      Starter.Shutdown;

      return False;
   end Delete;

   -----------------
   -- Delete_View --
   -----------------

   function Delete_View (Widget : access Gtk_Widget_Record'Class;
                         Event  :        Gdk.Event.Gdk_Event_Expose;
                         Vv     :        Visor_Plus_View)
                         return          Boolean
   is
      pragma Unreferenced (Widget, Event);
      use Widget_Lists;
      use type Visor_Widget.Object_Access;

      Target : Cursor := First (Vv.Visor_Access.Widgets);
      Aux    : Visor_Widget.Object_Access;
   begin
      while Has_Element (Target) and then Element (Target).Widget /= Vv.View_Access loop
         Next (Target);
      end loop;

      if Has_Element (Target) then
         Hide (Element (Target).Window);
         Aux := Element (Target).Widget;
         Visor_Widget.Free (Aux);
         Vv.Visor_Access.Widgets.Delete (Target);
      else
         Log ("Destroying view: Widget not found?", Error);
         Log ("List Len:" & Vv.Visor_Access.Widgets.Length'Img, Always);
      end if;

      return False;
   end Delete_View;

   -------------------
   -- Clear_Clicked --
   -------------------

   procedure Clear_Clicked (Widget : access Gtk_Widget_Record'Class;
                            Event  :        Gdk.Event.Gdk_Event_Expose;
                            Visor  :        Object_Access)
   is
      pragma Unreferenced (Widget, Event);
   begin
      Visor.Clear_Tasks;
   end Clear_Clicked;

   ------------------
   -- Stop_Clicked --
   ------------------

   procedure Stop_Clicked (Widget : access Gtk_Widget_Record'Class;
                           Event  :        Gdk.Event.Gdk_Event_Expose;
                           Visor  :        Gui.Visor.Object_Access)
   is
      pragma Unreferenced (Widget, Event);
   begin
      Visor.Emergency_Stop;
   end Stop_Clicked;

   ------------------
   -- View_Clicked --
   ------------------

   procedure View_Clicked (Widget  : access Gtk_Widget_Record'Class;
                           Visor   :        Object_Access)
   is
      Item : constant Gtk_Menu_Item_With_View := Gtk_Menu_Item_With_View (Widget);
   begin
      Create_View (Visor.all, +Item.Name);
   end View_Clicked;

   ------------------
   -- Is_Recording --
   ------------------

   function Is_Recording (This : Object) return Boolean is
   begin
      return This.Top /= null and then Get_Active (This.Top.Record_Button);
   end Is_Recording;

   -------------------
   -- Capture_Views --
   -------------------

   procedure Capture_Views (This : in out Object) is
      use Agpl.Strings;
      use Widget_Lists;
      procedure Do_It (I : Cursor) is
      begin
         Agpl.Gdk.Snapshot.Save_Png
           (Get_Window (Element (I).Area),
            External_Tag
              (Element (I).Widget.all'Tag) & "." &
               Rpad (Trim (This.Recording_Index'Img), 5, '0') & ".png");
      end Do_It;
   begin
      This.Widgets.Iterate (Do_It'Access);
   end Capture_Views;

   --------
   -- Id --
   --------

   function Id (This : Object) return Node_Id is
   begin
      return This.Config.Id;
   end Id;

end Sancta.Gui.Visor;
