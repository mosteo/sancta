with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Trace is

   -------------
   -- Message --
   -------------

   function Message (Text    : in String;
                     Level   : in Levels;
                     Section : in String) return Gui.Robot_Data.Network_Update is
   begin
      return Gui.Robot_Data.Network_Update'
        (Network.Message with
         Kind     => Gui.Robot_Data.Trace,
         Text     => +Text,
         Level    => Level,
         Section  => +Section);
   end Message;

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "")
   is
   begin
      This.Link.Send
        (This.Dest.all,
         Message
           (This.Decorate (Text, Level, Section),
            Level,
            Section));
   end Log;

end Sancta.Trace;
