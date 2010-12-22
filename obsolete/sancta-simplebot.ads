 

--  Simplebot: just obeys orders.
--  Accepts Set_Tasks, Clear_Tasks messages.

with Sancta.Netbot;
with Sancta.Network;

with Agpl.Chronos;

package Sancta.Simplebot is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "Simplebot";
   Detail_Section : constant String := "Simplebot.detail";
   --  To selectively enable debug messages...

   type Object is new Netbot.Object with private;

   procedure Init (This : in out Object);
   --  Subscription to required messages.

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);
   --  Do something with a received packet.
   --  Override for the packets of your interest.

   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Done should be true when the bot has finished operation.
   --  Once this happens Run shouldn't be called again.

private

   type Object is new Netbot.Object with
      record
         Hello_Cron : Agpl.Chronos.Object;
         Wait_Cron  : Agpl.Chronos.Object;

         Shutdown   : Boolean := False; -- Set to true when shutdown due.
      end record;

end Sancta.Simplebot;
