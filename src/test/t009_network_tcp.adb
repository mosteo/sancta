with
Ada.Streams,
     Agpl.Trace,
     Agpl.Trace.Utils,
     Agpl.Xml,
     Sancta.Network,
     Sancta.Network.Inbox,
     Sancta.Network.Layer.Tcp,
     T000_Types;

use Ada.Streams,
    Agpl,
    Agpl.Trace,
    Sancta,
    Sancta.Network,
    T000_Types;

procedure T009_Network_Tcp is

   Channel : constant Network.Channel := Network.Value ("4chan");

   Xml_Config : constant String :=
                  "" &
   "<t007>" &
   "   <group id='all'>" &
   "      <component name='network' kind='udp'>" &
   "         <node id='ari' address='127.0.0.1:7001'/>" &
   "         <node id='ben' address='127.0.0.2:7002'/>" &
   "         <node id='ced' address='127.0.0.3:7003'/>" &
   "      </component>" &
   "   </group>" &
   "" &
   "   <agent id='ari'>" &
   "      <group>all</group>" &
   "   </agent>" &
   "   <agent id='ben'>" &
   "      <group>all</group>" &
   "   </agent>" &
   "   <agent id='ced'>" &
   "      <group>all</group>" &
   "   </agent>" &
   "</t007>";

   Id  : constant Node_Id := Value ("ari");

   Conf : constant Xml.Document := Xml.From_String (Xml_Config);

   Net : aliased Sancta.Network.Layer.Tcp.Object;

   Box : aliased Network.Inbox.Object;
begin
   Trace.Set_Decorator (Trace.Utils.Prepend_Level_Timestamp_Section'Access);
   Trace.Set_Level (Trace.Debug);
   Trace.Enable_Section (Sancta.Network.Layer.Tcp.Log_Section);

   Log ("Starting...", Debug);

   Net.Set_Id (Id);
   Net.Initialize
     (Xml.Get_With_Attribute
        (Xml.Get (Conf, "group"),
         "component",
         "name",
         "network"));

   Net.Subscribe (Box'Unchecked_Access, Channel);

   loop
      delay 0.2;
      Log ("TOP-LEVEL SEND", Always);
      Net.Send (New_Address (Id, Channel), Create);
      delay 0.2;

      while not Box.Is_Empty loop
         declare
            M : constant Check_Message := Check_Message (Box.Get_First);
         begin
            M.Selfcheck;
            Log ("Selfcheck passed", Always);
            Box.Remove_First;
         end;
      end loop;
   end loop;
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end T009_Network_Tcp;
