with Sancta.Gui.Canvas;

package Sancta.Gui.Main is

   Shutdown : Boolean := False;
   pragma Atomic (Shutdown);
   --  Set it to true to inform that the GUI has requested exit.

   procedure Add_Canvas (C : in Canvas.Object);

   procedure Display_Canvas (I : in Natural);
   --  Set in view the Nth canvas.

   procedure Start;
   --  Create the GUI, but no main loop thread is create. You must manually
   --  call the "Update" procedure frequently to update the gui.

   procedure Update;
   --  Process pending events from the GUI

end Sancta.Gui.Main;
