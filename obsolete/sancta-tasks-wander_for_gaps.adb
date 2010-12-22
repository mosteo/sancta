--  with Sancta.Gui.Callbacks;
with Sancta.Tasks.Types;

with Text_Io; use Text_Io;

package body Sancta.Tasks.Wander_For_Gaps is

   -------------------
   -- Create_Widget --
   -------------------

   procedure Create_Widget (This   : in out Object;
                            Widget :    out Gtk_Widget) is
   begin
      if not This.Inited then
         This.Inited := True;

         --  Create the gui store:
         declare
            use Sancta.Gui.User_Data_Handle;
         begin
            Bind (This.Gui_Data, new Tasks.Types.Wander_For_Gaps_Info);
         end;

         --  Create and connect a console for drawing gaps.
         Gtk_New (This.Area);

         Put_Line ("Gap drawing area created.");

         declare
            package Cb renames Gui.Callbacks.Handlers;

            procedure Connect is
            begin
               Cb.Connect
                 (This.Area,
                  "expose_event",
                  Cb.To_Marshaller
                    (Gui.Callbacks.Expose_Gaps'Access),
                  This.Gui_Data);
               Put_Line ("Gap drawing area callback connected.");
            end Connect;
         begin
            Connect;
         end;

      end if;

      Widget := Gtk_Widget (This.Area);
   end Create_Widget;

   ------------
   -- Redraw --
   ------------

   procedure Redraw (This : in out Object) is
   begin
      Queue_Draw (This.Area);
   end Redraw;

end Sancta.Tasks.Wander_For_Gaps;
