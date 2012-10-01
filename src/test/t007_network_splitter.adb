with
Ada.Streams,
     Agpl.Trace,
     Agpl.Trace.Utils,
     Agpl.Xml,
     Sancta.Network,
     Sancta.Network.Inbox,
     Sancta.Network.Layer.Splitter,
     Sancta.Network.Layer.Udp,
     T000_Types;

use Ada.Streams,
    Agpl,
    Agpl.Trace,
    Sancta,
    Sancta.Network,
    T000_Types;

procedure T007_Network_Splitter is

   package Udp_Split is new Sancta.Network.Layer.Splitter
     (Sancta.Network.Layer.Udp.Object, 128);

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

   Id  : constant Node_Id := Sancta.Value ("ari");

   Conf : constant Xml.Document := Xml.From_String (Xml_Config);

   Netsplit : aliased Udp_Split.Object;

   Box : aliased Network.Inbox.Object;
begin
   Trace.Set_Decorator (Trace.Utils.Prepend_Level_Timestamp_Section'Access);
   Trace.Set_Level (Trace.Debug);
   Trace.Enable_Section (Udp_Split.Log_Section);

   Log ("Starting...", Debug);

   Netsplit.Set_Id (Id);
   Netsplit.Initialize
     (Xml.Get_With_Attribute
        (Xml.Get (Conf, "group"),
         "component",
         "name",
         "network"));

   Netsplit.Subscribe (Box'Unchecked_Access, Channel);

   loop
      delay 0.1;
      Netsplit.Send (New_Address (Id, Channel), Create);

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
end T007_Network_Splitter;
