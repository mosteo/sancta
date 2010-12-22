with Agpl.Task_Termination,
     Agpl.Trace,
     Agpl.Trace.Utils,
     Agpl.Xml,
     Sancta.Network,
     Sancta.Network.Layer.Root,
     Sancta.Network.Layer.Udp,
     T000_Types;

use Agpl,
    Agpl.Trace,
    Sancta,
    T000_Types;

procedure T005_Bw is

   Xml_Config : constant String :=
                  "" &
   "<t004>" &
   "   <group id='all'>" &
   "      <component name='network' kind='udp'>" &
   "         <node id='ari' address='127.0.0.1:7001'/>" &
   "         <node id='ben' address='127.0.0.2:7002'/>" &
   "      </component>" &
   "   </group>" &
   "" &
   "   <agent id='ari'>" &
   "      <group>all</group>" &
   "   </agent>" &
   "   <agent id='ben'>" &
   "      <group>all</group>" &
   "   </agent>" &
   "</t004>";

   Nodes : constant array (1 .. 2) of Node_Id := (Value ("ari"), Value ("ben"));

   task type Tester (Num : Positive) is
      entry Start (Id : Sancta.Node_Id);
   end Tester;

   task body Tester is
      Id  : Node_Id;
      Net : aliased Network.Layer.Udp.Object;

      Conf : constant Xml.Document := Xml.From_String (Xml_Config);
   begin
      accept Start (Id : Sancta.Node_Id) do
         Tester.Id := Id;
      end Start;

      Net.Set_Id (Id);
      Net.Initialize
        (Xml.Get_with_attribute
           (Xml.Get (Conf, "group"),
            "component",
            "name",
            "network"));

      loop
         delay 1.0;

         if Id = Nodes (Nodes'First) then
            Net.Send (Network.New_Address (Nodes (Nodes'Last),
                                           Network.Value ("chan")),
                      Filler'(Network.Message with Last => 1000, others => <>));
         end if;
      end loop;

   exception
      when E : others =>
         Log ("Tester" & Num'Img & ": " & Report (E), Error);
   end Tester;

   type Tester_Access is access all Tester;

   Ta : Tester_Access;

begin
   Trace.Set_Decorator (Trace.Utils.Prepend_Level_Timestamp_Section'Access);
   Trace.Set_Level (Trace.Debug);
   Trace.Enable_Section (Sancta.Network.Layer.Root.Bw_Section);
   Trace.Enable_Section (Sancta.Network.Layer.Root.Log_Section);

   Log ("Starting...", Debug);

   for I in Nodes'Range loop
      Ta := new Tester (I);
      Ta.Start (Nodes (I));
   end loop;
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end T005_Bw;
