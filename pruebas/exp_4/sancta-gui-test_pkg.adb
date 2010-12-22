with Text_Io; use Text_Io;

package body Sancta.Gui.Test_Pkg is

   --------------------
   -- Expose_Handler --
   --------------------

   function Expose_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Expose)
      return Boolean
   is
   begin
      Put_Line ("Expose event 1");
      return true;
   end Expose_Handler;

   function Expose_Handler2
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Expose)
      return Boolean
   is
   begin
      Put_Line ("Expose event 2");
      return true;
   end Expose_Handler2;

end Sancta.Gui.Test_Pkg;
