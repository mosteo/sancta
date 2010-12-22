package body Sancta.Netlistener is

   procedure Initialize (This : in out Object) is
   begin
      if not (This in Object) then -- To avoid infinite recursion
         Init (Object'Class (This));
      end if;
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object)
   is
      use Callback_Maps;
   begin
      pragma Assert (This.Subscribed, "Unsuscribed listener asked to run");
      Log ("Within Run", Debug, Detail_Section);

      while not This.Mailbox.Is_Empty loop
         declare
            procedure Read (Mess : Network.Message'Class;
                            Meta : Network.Message_Metadata)
            is
            begin
               Log ("Processing msg: " & External_Tag (Mess'Tag),
                    Debug, Log_Section);

               Process_Incoming_Packet (Object'Class (This), Mess, Meta);

               if This.Callbacks.Contains (External_Tag (Mess'Tag)) then
                  Element (This.Callbacks.Find (External_Tag (Mess'Tag)))
                    (This, Mess, Meta); -- <-- Call to registered callback
               end if;
            end Read;
         begin
            This.Mailbox.Open (Read'Access, Block => False);
         end;
      end loop;
   end Run;

   --------------
   -- Register --
   --------------

   procedure Register (This : in out Object;
                       Kind : in     Ada.Tags.Tag;
                       Cb   : in     Callback)
   is
   begin
      This.Callbacks.Include (External_Tag (Kind), Cb);
   end Register;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This : in out Object;
                        Chan : in     Network.Channel)
   is
   begin
      This.Link.Subscribe (This.Mailbox'Unchecked_Access, Chan);
      This.Subscribed := True;
   end Subscribe;

end Sancta.Netlistener;
