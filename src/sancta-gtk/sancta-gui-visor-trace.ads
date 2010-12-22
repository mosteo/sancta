with Agpl.Trace.Root;

--  Specific tracer for the visor, so he can log its own messages!

package Sancta.Gui.Visor.Trace is

   pragma Elaborate_Body;

   subtype Levels is Agpl.Trace.Levels;

   type Object (Parent : access Visor.Object'Class) is
     new Agpl.Trace.Root.Object with null record;

   type Object_Access is access all Object;

   overriding
   procedure Log (This    : in out Object;
                  Text    : in     String;
                  Level   : in     Levels;
                  Section : in     String := "");

end Sancta.Gui.Visor.Trace;
