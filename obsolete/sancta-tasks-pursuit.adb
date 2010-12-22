with Sancta.Debug;
with Sancta.Gui.Callbacks;
with Sancta.Tasks.Types;

with Gdk.Event;  use Gdk.Event;
with Gtk.Widget; use Gtk.Widget;

package body Sancta.Tasks.Pursuit is

   ------------
   -- Create --
   ------------

   function Create
     (Start : in Sancta.Types.Pose;
      Wait  : in Boolean := False)
      return Object
   is
      This : Object;
   begin
      This.Must_Wait := Wait;
      This.Start     := Start;

      --  Create the shared store:
      declare
         use Types.Smart_Pursuit_Info;
      begin
         Bind (This.Data, new Tasks.Types.Pursuit_Info);

         --  Set first goal:
         Ref (This.Data).Goals.Include ("", Start);
      end;

      return This;
   end Create;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (This : in Object) return Types.Smart_Pursuit_Info.Object is
   begin
      return This.Data;
   end Get_Data;

   ---------------
   -- Get_Start --
   ---------------

   function Get_Start (This : in Object) return Pose is
   begin
      return This.Start;
   end Get_Start;

   ----------------
   -- Get_Widget --
   ----------------

   procedure Get_Widget (This : in out Object; Widget : out Gtk_Widget) is
   begin
      --  Create and connect a console for custom drawing.
      Gtk_New (This.Area);

      declare
         package Cb renames Gui.Callbacks.Handlers_Pursuit;

         procedure Connect is
         begin
            Set_Events (This.Area,
                        Get_Events (This.Area) + Button_Press_Mask);


            Cb.Connect
              (This.Area,
               "button_press_event",
               Cb.To_Marshaller
                 (Gui.Callbacks.Pursuit_Click'Access),
               This.Data);

            Cb.Connect
              (This.Area,
               "expose_event",
               Cb.To_Marshaller
                 (Gui.Callbacks.Pursuit_Expose'Access),
               This.Data);
         end Connect;
      begin
         Connect;
      end;

      Widget := Gtk_Widget (This.Area);
      declare
         use Types.Smart_Pursuit_Info;
      begin
         Ref (This.Data).Widget := Widget;
      end;
   end Get_Widget;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (This : in out Object) is
   begin
      Queue_Draw (This.Area);
   end Redraw;

   ----------------------
   -- Set_Working_Area --
   ----------------------

   procedure Set_Working_Area
     (This : in out Object;
      P1,
      P2   : in     Sancta.Types.Pose)
   is
      use Tasks.Types;
      use Tasks.Types.Smart_Pursuit_Info;
      PData : Pursuit_Info renames Ref (This.Data).all;
   begin
      PData.P1 := P1;
      PData.P2 := P2;
   end Set_Working_Area;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return "Pursuit starting at " & Debug.To_String (This.Start);
   end To_String;

end Sancta.Tasks.Pursuit;
