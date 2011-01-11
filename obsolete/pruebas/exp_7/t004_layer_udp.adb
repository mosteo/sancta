with Sancta.Network;
with Sancta.Network.Layer.Udp;

with Agpl.Trace; use Agpl.Trace;
with Agpl.Xml;

with Ada.Streams; use Ada.Streams;
with Text_Io; use Text_Io;

procedure T004_Layer_Udp is

   Config : constant Agpl.Xml.Document :=
              Agpl.Xml.Parse ("/home/jano/prog/doctor/sancta/t004_layer_udp.xml");

   task type Endpoint (Id    : access constant String;
                       Other : access constant String);

   task body Endpoint is
      Link   : Sancta.Network.Layer.Udp.Object;
      Me, It : Sancta.Network.Node_Id;
      Data   : constant Stream_Element_Array := (1 .. 10 => 16#bf#);
      Ack    : Boolean;
   begin
      Me := Sancta.Network.Value (Id.all);
      It := Sancta.Network.Value (Other.all);

      Link.Init (Sancta.Network.Value (Id.all), Config);

      delay 1.0;
      Link.Send (It, Data);

      delay 2.0;
      Link.Send (It, Data, 1.0, Ack);
      pragma Assert (Ack);

      delay 2.0;

      Put_Line ("Shutting down " & Id.all);
      Link.Shutdown;
      Put_Line (Id.all & " shut down");
   exception
      when E : others =>
         Put_Line ("Exception at " & Id.all & ": " & Report (E));
   end Endpoint;

   Tracer : aliased Agpl.Trace.Object;

   Ari    : aliased constant String := "Ari";
   Ben    : aliased constant String := "Ben";

begin
   Create (Tracer,
           Minimum_Level => Debug,
           Console_Echo  => True);

   Set_Default_Tracer (Tracer'Unrestricted_Access);

   declare
      T1 : Endpoint (Ari'Access, Ben'Access);
      T2 : Endpoint (Ben'Access, Ari'Access);
   begin
      null;
   end;
exception
   when E : others =>
      Put_Line ("Exception: " & Report (E));
end T004_Layer_Udp;
