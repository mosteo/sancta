with Agpl.Generic_Handle;
with Agpl.Tasking.Code;
with Agpl.Tasking.Workers;

package body Sancta.Network.Layer is

   package Addr_Handle is new Agpl.Generic_Handle (Address);
   package Msg_Handle  is new Agpl.Generic_Handle (Message'Class);

   type Sender is new Agpl.Tasking.Code.Object with record
      Link : Object_Access;
      Dest : Addr_Handle.Object;
      Data : Msg_Handle.Object;
      Wait : Milliseconds;
   end record;

   overriding
   procedure Run (This : in out Sender);

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Sender) is
   begin
      if This.Wait /= 0 then
         Log ("Send_Async: Waiting" & This.Wait'Img & " ms...", Debug, Log_Section);
         delay Duration (This.Wait) / 1000.0;
      end if;

      Log ("Send_Async: Sending now...", Debug, Log_Section);
      This.Link.Send (This.Dest.Ref.all, This.Data.Ref.all);
   end Run;

   ----------------
   -- Send_Async --
   ----------------

   procedure Send_Async
     (This : in out Object'Class;
      Dest : in     Address;
      Data : in     Message'Class;
      Wait : in     Milliseconds := 0;
      Stack : in     Natural      := 2 * Max_Message_Size)
   is
   begin
      Agpl.Tasking.Workers.Launch
        (Sender'
           (This'Unchecked_Access,
            Addr_Handle.Set (Dest),
            Msg_Handle.Set  (Data),
            Wait),
         "send_async",
         Stack => Stack,
         Activate => False);
   end Send_Async;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out Object'Class;
                   Chan :        Channel;
                   Data :        Message'Class)
   is
   begin
      This.Send (New_Address (Chan), Data);
   end Send;

end Sancta.Network.Layer;
