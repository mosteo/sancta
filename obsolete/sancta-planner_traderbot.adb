 

with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Tasks.Choose_Entry_Point;

--  pragma Warnings (Off);
--  with Sancta.Tasks.Used;
--  pragma Warnings (On);

--  with Sancta.Tasks.Containers;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Planner_Traderbot is

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      This.Subscribe (Network.Groups.Channel_Tags (Network.Groups.Management_Channel));
   end Init;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
   begin
      if M in Network.Messages.Propose_Task_Type'Class then
         declare
            X : Network.Messages.Propose_Task_Type renames
              Network.Messages.Propose_Task_Type (M);
         begin
            if X.Job.Get in Tasks.Choose_Entry_Point.Object then
               Log ("Discarded un-auctionable task: " & X.Job.Get.To_String, Debug,
                    Section => Log_Section);
            else
               Log ("Received task: " & X.Job.Get.To_String, Debug,
                    Section => Log_Section);
               This.Seller.Add_Task (X.Job.Get,
                                     Lasts   => 1.0,
                                     Discard => False,
                                     Must    => True);
            end if;
         end;
      end if;
   end Process_Incoming_Packet;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean)
   is
--        use Sancta.Tasks.Containers;
--        Discarded : List;
   begin
      Netlistener.Run (Netlistener.Object (This));
      This.Seller.Run;
   end Run;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration (This        : in out Object;
                                Config      : in     Xml.Document;
                                Cost_Policy : in Auctioner.Cost_Policies :=
                                  Auctioner.Full_Cost)
   is
   begin
      This.Seller.Set_Configuration (Config, Cost_Policy);
   end Set_Configuration;

end Sancta.Planner_Traderbot;
