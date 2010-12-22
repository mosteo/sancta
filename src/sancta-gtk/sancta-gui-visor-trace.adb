package body Sancta.Gui.Visor.Trace is

   procedure Log (This    : in out Object;
                  Text    : in     String;
                  Level   : in     Levels;
                  Section : in     String := "")
   is
   begin
      if This.Must_Log (Level, Section) then
         Local_Log (This.Parent.all,
                    Text    => Text,
                    Level   => Level,
                    Section => Section,
                    Sender  => Image (This.Parent.Id));
      end if;
      Agpl.Trace.Root.Object (This).Log (Text, Level, Section);
   end Log;

end Sancta.Gui.Visor.Trace;
