with
     Agpl.Trace,
     Agpl.Trace.Utils,
     Sancta.Component.Network,
     Sancta.Component.Network_Rtwmp,
     Sancta.Config,
     Sancta.Component.Include,
     Sancta.Datastore,
     Sancta.Network,
     Sancta.Network.Inbox,
     Sancta.Network.Layer,
     Sancta.Starter,
     T000_Types;

use
    Agpl,
    Agpl.Trace,
    Sancta,
    Sancta.Network,
    T000_Types;

procedure T001_Rtwmp is

   Channel : constant Network.Channel := Network.Value ("4chan");
   Box     :  aliased Network.Inbox.Object;

   The_End : Boolean := False;
   pragma Atomic (The_End);

   task Checker;
   task body Checker is
   begin
      while not The_End loop
         while not Box.Is_Empty loop
            declare
               Msg : constant Check_Message :=
                       Check_Message (Box.Get_First);
               Mdt : constant Network.Message_Metadata :=
                       Box.Get_First_Metadata;
            begin
               Log ("Incoming" & Msg.Length'Img & " bytes from " &
                    Sancta.Image (Mdt.Sender), Always);
               Msg.Selfcheck;
               Log ("Selfcheck passed", Always);
               Box.Remove_First;
            end;
         end loop;
         delay 0.01;
      end loop;
   end Checker;

begin
   Sancta.Component.Include.Register;
   Sancta.Component.Network_Rtwmp.Register;

   Trace.Set_Decorator (Trace.Utils.Prepend_Level_Timestamp_Section'Access);
   Trace.Set_Level (Trace.Debug);

   Log ("Starting...", Debug);

   Sancta.Starter.Launch;

   declare
      Link : constant access Network.Layer.Object'Class :=
               Component.Network.Network
                 (Datastore.Object (Config.Get_Id).Get ("link")).Link;
   begin

      Link.Subscribe (Box'Unchecked_Access, Channel);

      Log ("Starting up...", Always);

      loop
         delay 0.2;
         Link.Send (New_Address (Channel), Create (1, 255));
      end loop;

      Log ("Sending 5 small packets...", Always);
      delay 5.0;
      for I in 1 .. 5 loop
         Link.Send (New_Address (Channel), Create (1, 255));
         delay 1.0;
      end loop;

      Log ("Sending 5 large packets...", Always);
      delay 5.0;
      for I in 1 .. 5 loop
         Link.Send (New_Address (Channel), Create (1000, 5000));
         delay 1.0;
      end loop;
   end;

exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end T001_Rtwmp;
