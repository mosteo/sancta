with Agpl.Chronos,
     Agpl.Random,
     Agpl.Task_Termination,
     Agpl.Trace,
     Agpl.Trace.Utils,
     Agpl.Xml,
     Sancta.Auctioneer,
     Sancta.Auctions,
     Sancta.Bidder,
     Sancta.Network,
     Sancta.Network.Layer.Root,
     Sancta.Network.Layer.Udp,
     T004_Auctions_Types;

use Agpl,
    Agpl.Trace,
    Sancta,
    T004_Auctions_Types;

procedure T004_Auctions is

   Channel : constant Network.Channel := Network.Value ("4chan");

   Xml_Config : constant String :=
                  "" &
   "<t004>" &
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
   "</t004>";

   task type Tester (Num : Positive) is
      entry Start (Id : Sancta.Node_Id);
   end Tester;

   task body Tester is
      Id  : Node_Id;
      Net : aliased Network.Layer.Udp.Object;
      Auc : Auctioneer.Object (Net'Access);
      Bdr : T004_Auctions_Types.Bidder (Net'Access);

      Conf : constant Xml.Document := Xml.From_String (Xml_Config);

      Started : Chronos.Object;
      Done    : Boolean := False;
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

      Auc.Create (Channel);
      Bdr.Create (Channel);

      loop
         Bdr.Run;
         Auc.Run;
         delay 0.01;

         if Started.Elapsed < 10.0 and then Random.Uniform < 0.01 then
            Auc.Add_Item (Item'(Auctions.Items with others => <>));
         elsif Started.Elapsed > 10.0 and not Done then
            Log ("ADITIONS ENDED", Always);
            Done := True;
         end if;
      end loop;

   exception
      when E : others =>
         Log ("Tester" & Num'Img & ": " & Report (E), Error);
   end Tester;

   type Tester_Access is access all Tester;

   Nodes : constant array (1 .. 3) of Node_Id :=
             (Value ("ari"), Value ("ben"), Value ("ced"));

   Ta : Tester_Access;

begin
   Trace.Set_Decorator (Trace.Utils.Prepend_Level_Timestamp_Section'Access);
   Trace.Set_Level (Trace.Debug);
   Trace.Enable_Section (Auctioneer.Log_Section);
   Trace.Enable_Section (Sancta.Bidder.Log_Section);
   Trace.Enable_Section (Sancta.Network.Layer.Root.Bw_Section);

   Log ("Starting...", Debug);
   Log ("For real...", Debug);

   for I in Nodes'Range loop
      Ta := new Tester (I);
      Ta.Start (Nodes (I));
   end loop;
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end T004_Auctions;
