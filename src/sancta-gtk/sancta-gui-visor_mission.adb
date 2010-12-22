with Gtk.Box,
     Gtk.Button,
     Sancta.Containers,
     Sancta.Gui.Visor_Widget.Handlers_U,
     Sancta.Tasks.Handle,
     Sancta.Tasks.Utils;

with Agpl.Trace;

package body Sancta.Gui.Visor_Mission is

   type Tasked_Button_Record is
     new Gtk.Button.Gtk_Button_Record with
      record
         Job : Tasks.Handle.Object;
      end record;

   type Tasked_Button is access all Tasked_Button_Record'Class;

   -------------
   -- Clicked --
   -------------

   procedure Clicked
     (Widget : access Gtk_Widget_Record'Class;
      This   : Visor_Widget.Object_Access)
   is
      But  : constant Tasked_Button := Tasked_Button (Widget);
      Thix : constant Object_Access := Object_Access (This);
   begin
      Log ("Manually launching task: " & But.Job.Ref.Image,
           Always, Log_Section);
      Thix.Data.Add_Task_For_Auction (But.Job.Ref.all);
   end Clicked;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (This : in Object)
      return Gtk_Widget
   is
   begin
      return This.Widget;
   end Get_Widget;

   --------------------
   -- Create_Buttons --
   --------------------

   procedure Create_Buttons (This : in out Object) is
      use Sancta.Containers;
      T : constant TC.Vectors.Vector :=
            Sancta.Tasks.Utils.To_Vector
              (This.Data.Get_Mission.Get_Plan.Enumerate_Tasks
                 (Pending   => True,
                  Primitive => True));

      Box : constant Gtk.Box.Gtk_Vbox := Gtk.Box.Gtk_Vbox (This.Widget);

      procedure Create_Button (I : Tc.Vectors.Cursor) is
         Job : constant Tasks.Object'Class := Tc.Vectors.Element (I);
         But : constant Tasked_Button := new Tasked_Button_Record;
      begin
         But.Initialize (Job.Image);
         But.Job.Set (Job);
         But.Set_Focus_On_Click (False);
         Box.Pack_End (But);
         Visor_Widget.Handlers_U.Connect
           (But,
            "clicked",
            Clicked'Access,
            Visor_Widget.Object_Access (This.This));
         Log ("Adding button for " & Job.Image, Always);
      end Create_Button;
   begin
      T.Iterate (Create_Button'Access);
   end Create_Buttons;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (This : in out Object;
      Data : access Visor_Data.Object)
   is
   begin
      This.Data := Data;

      This.Create_Buttons;
   end Set_Data;

   ------------
   -- Create --
   ------------

   function Create
     (Context : Visor_Factory.Creation_Context)
      return Visor_Widget.Object_Access
   is
      pragma Unreferenced (Context);
      use Gtk;
      This : constant Object_Access := new Object;
      Vbox : Gtk.Box.Gtk_Vbox;
   begin
      Box.Gtk_New_Vbox (Vbox, Homogeneous => True);
      Vbox.Unset_Focus_Chain;

      This.Widget := Gtk_Widget (Vbox);
      return Visor_Widget.Object_Access (This);
   end Create;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Visor_Factory.Register (Create'Access, View_Name);
   end Register;

end Sancta.Gui.Visor_Mission;
