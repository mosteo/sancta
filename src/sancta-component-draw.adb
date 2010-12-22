with Agpl.Drawing;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Network.Messages;

package body Sancta.Component.Draw is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      This.Drawer := Ctypes.Drawer (This.Input (Requires_Drawer)).Drawer;

      if This.Provided (Requires_Drawable) then
         This.Subscribe (Requires_Drawable);
         This.Verify    (Option_Label);
      end if;

      if This.Provided (Requires_Link) then
         This.Link := Sancta.Network.Layer.Object_Access
           (Component.Network.Network
              (This.Input (Requires_Link)).Link);
         This.Link.Subscribe
           (This.Inbox'Access,
            Sancta.Network.Value (This.Option (Option_Channel)));
      end if;

      return Component.Object_Access (This);
   end Create;

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      Next := Clock + 0.1;

      while not This.Inbox.Is_Empty loop
--         Log ("MESSAGE IN INBOX", Always);
         declare
            M : constant Sancta.Network.Message'Class := This.Inbox.Get_First;
         begin
            This.Inbox.Remove_First;
            if M in Sancta.Network.Messages.Redirect_Type'Class then
               declare Mr : Sancta.Network.Messages.Redirect_Type'Class renames
                    Sancta.Network.Messages.Redirect_Type'Class (M);
                  use Agpl.Drawing;
               begin
                  Log ("RECEIVED REDIRECT: " & String (Mr.Key.Ref.all),
                       Debug, Log_Section);
                  if Mr.Val.Ref.all in Drawable'Class then
                     Log ("DRAWING REDIRECT: " & String (Mr.Key.Ref.all),
                          Debug, Log_Section);
                     This.Drawer.Draw
                       (Mr.Key.Get,
                        Drawable'Class (Mr.Val.Ref.all));
                  end if;
               end;
            else
               Log ("Non-Redirect was: " & External_Tag (M'Tag),
                    Warning, Log_Section);
            end if;
         end;
      end loop;
      if This.Provided (Requires_Link) then
         Log ("INBOX EMPTIED", Never);
      end if;
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
   begin
      if Value in Agpl.Drawing.Drawable'Class then
         Log ("DRAWING STORED: " & String (This.EKey (Key)),
              Debug, Log_Section);
         This.Drawer.Draw (This.Option (Option_Label),
                           Agpl.Drawing.Drawable'Class (Value));
      else
         Log ("Object supplied not in Drawable'Class", Warning, Log_Section);
      end if;
   end Key_Stored;

end Sancta.Component.Draw;
