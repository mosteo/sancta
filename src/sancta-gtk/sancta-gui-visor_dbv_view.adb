--  with Sancta.Cost_Proxy;
with Sancta.Distributed;
with Sancta.Distributed.Datastore;
with Sancta.Distributed.Types;
-- with Sancta.Draw;
with Sancta.Gui.Visor_Factory; pragma Elaborate_All (Sancta.Gui.Visor_Factory);
with Sancta.Gui.Visor_Widget.Handlers_UR;

with Sancta;
with Sancta.Assignment;
with Sancta.Assignment.Utils;
with Agpl.Trace;
with Agpl; use Agpl;

with Gdk.Drawable; use Gdk.Drawable;
with Gdk.Window;   use Gdk.Window;

with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Gui.Visor_Dbv_View is

   use type Sancta.Costs;
   use type Visor_Data.Object_Access;
   use type Visor_Data.Robot_Access;

   ------------
   -- Create --
   ------------

   function Create (Context : Visor_Factory.Creation_Context)
                    return    Visor_Widget.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      Gtk_New (This.Area);

      Set_Flags (This.Area, Can_Focus);
      --  To allow keyboard interaction

      --  Connect events
      Visor_Widget.Handlers_Ur.Connect
        (This.Area,
         "expose-event",
         Visor_Widget.Handlers_Ur.To_Marshaller (Expose'Access),
         Visor_Widget.Object_Access (This));

      return Visor_Widget.Object_Access (This);
   exception
      when E : others =>
         Log ("General_View.Create: " & Report (E), Error);
         raise;
   end Create;

   ------------
   -- Expose --
   ------------

   function Expose (Widget : access Gtk_Widget_Record'Class;
                    Event  :        Gdk.Event.Gdk_Event_Expose;
                    This   :        Visor_Widget.Object_Access)
                    return          Boolean
   is
      pragma unreferenced (Event);
      use Agpl.Gdk.Palette;
      use Agpl.Gdk.Drawer;
      X : constant Visor_Dbv_View.Object_Access :=
            Visor_Dbv_View.Object_Access (This);
      W : constant Gdk_Drawable := Get_Window (X.Area);
      D : Agpl.Gdk.Drawer.Object renames X.Drawer;

      White : constant String := "#ffffff";

      Database : constant Distributed.Datastore.Object_Access := null;
--                   Sancta.Plugin.Shared_Database.Get;
      use type Distributed.Datastore.Object_Access;

      function Bkcolor (Bot : in String) return String is
         type Bots is (Ari, Ben, Ced, Dan);
         Colors : constant array (Bots) of String (1 .. 7) :=
                    (Ari => "#ffaaaa", Ben => "#aaffaa",
                     Ced => "#aaaaff", Dan => "#ffaaff");
      begin
         return Colors (Bots'Value (Bot));
      exception
         when others =>
            return White;
      end Bkcolor;
   begin

      Set_Drawable (X.Pal, W);
      D.Set_Drawable (W);

--        if Database /= null then
--           declare
--              Shared_Context_Key : Distributed.Object_Key renames
--                Plugin.Annealer.Shared_Context_Key;
--              Context            : Distributed.Types.Danneal;
--              Ass                : Sancta.Assignment.Object;
--              Costs              : Cost_Proxy.Object;
--           begin
--              if Database.Contains (Shared_Context_Key) then
--                 Context :=
--                   Distributed.Types.Danneal (Database.Get (Shared_Context_Key));
--
--                 Set_Background
--                   (W, Get_Color
--                      (X.Pal'Access, Bkcolor (Network.Image (Context.Found_By))));
--                 Clear (W);
--
--                 D.Draw_Begin;
--
--                 Ass := Context.History;
--                 Sancta.Assignment.Utils.Concatenate (Ass, Context.Assignment);
--
--                 Costs := Cost_Proxy.Object (Context.Agent_Costs.Get);
--                 Costs.Clear_All_History;
--                 --  Since we have concatenated past and future, history is not
--                 --  necessary (and wrong).
--
--                 Sancta.Draw.Draw_Assignment
--                   (Ass,
--                    D,
--                    X.Pal'Access,
--                    Gtk_Widget (Widget),
--                    Costs,
--                    Show_Costs => True);
--
--                 D.Draw_End;
--              else
--                 Set_Background (W, Get_Color (X.Pal'Access, White));
--                 Clear (W);
--              end if;
--           end;
--        else
--           Set_Background (W, Get_Color (X.Pal'Access, White));
--           Clear (W);
--        end if;

      return False;
   exception
      when E : others =>
         Put_Line ("Visor_General_View.Expose: " & Report (E));
         Log ("Visor_General_View.Expose: " & Report (E), Error);
         return False;
   end Expose;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (This : in Object) return Gtk_Widget is
   begin
      return Gtk_Widget (This.Area);
   end Get_Widget;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (This : in out Object;
                       Data : access Visor_Data.Object)
   is
   begin
      This.Data := Visor_Data.Object_Access (Data);
   end Set_Data;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Object;
                     Id   : in     Node_Id;
                     Msg  : in     Robot_Data.Network_Update)
   is
      pragma Unreferenced (Id);
      use Gui.Robot_Data;
   begin
      This.Redraw;

      case Msg.Kind is
         when others =>
            null;
      end case;
   end Update;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (This : in out Object) is
   begin
      if This.Redraw_Timer.Elapsed >= Minimum_Redraw_Wait then
         Queue_Draw (This.Area);
         This.Redraw_Timer.Reset;
      end if;
   end Redraw;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Visor_Factory.Register (Create'Access, "Shared DB visual view");
   end Register;

end Sancta.Gui.Visor_Dbv_View;
