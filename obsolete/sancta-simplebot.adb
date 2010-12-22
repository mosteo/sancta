 

--  Simplebot: just obeys orders.

with Sancta.Network.Groups;
with Sancta.Network.Messages;

--  pragma Warnings (Off);
--  with Sancta.Tasks.Used;
--  pragma Warnings (On);

with Agpl.Trace; use Agpl.Trace;

package body Sancta.Simplebot is

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      This.Subscribe (Network.Groups.Emergency_Channel);
      This.Subscribe (Network.Groups.Management_Channel);
   end Init;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean)
   is
   begin
      Netbot.Run (Netbot.Object (This), Done);

      --  Send a hello every second:
      if This.Hello_Cron.Elapsed >= 1.0 then
         This.Hello_Cron.Reset;

         This.Link.Multicast (Network.Groups.Emergency_Channel,
                              Network.Messages.Hello);
      end if;

      if This.Wait_Cron.Elapsed >= 5.0 and then not This.Bot.Has_Tasks then
         This.Wait_Cron.Reset;

         --  Log ("Waiting for tasks...", Debug, Section => Log_Section);
      end if;

      Done := This.Shutdown or else Done;
   end Run;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
   begin
      if M in Network.Messages.Shutdown_Type'Class then
         -- SHUTDOWN --
         This.Shutdown := True;
         Log ("Shutdown command received.", Informative);
      elsif M in Network.Messages.Set_Tasks_Type'Class then
         -- SET_TASKS --
         This.Bot.Set_Tasks (Network.Messages.Set_Tasks_Type (M).Jobs);
         Log ("Received" & This.Bot.Get_Tasks.Length'Img & " new tasks.",
              Debug,
              Section => Log_Section);
      end if;
   end Process_Incoming_Packet;

end Sancta.Simplebot;
