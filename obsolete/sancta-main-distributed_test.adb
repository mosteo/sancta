with Sancta.Aux;
with Sancta.Distributed;
with Sancta.Distributed.Datastore;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;

with Agpl.Chronos;
with Agpl.Random;
with Agpl.Task_Termination; pragma Elaborate_All (Agpl.Task_Termination);
with Agpl.Trace; use Agpl.Trace;
with Agpl.Trace.Utils;
with Agpl.Xml;
with Agpl; use Agpl;

with Ada.Command_Line; use Ada.Command_Line;

--  1st argument: Node id

procedure Sancta.Main.Distributed_Test is
   Id    : aliased Node_Id   := Network.Value (Argument (1));
   Link  : aliased Network.Layer.Udp.Object (Id'Access);
   Store : aliased Distributed.Datastore.Object (Link'Access);

   subtype Int is Aux.Int;

   use type Distributed.Object_Key;

   I     : Int := (Distributed.Object_Data with 1);
   Ki    : constant Distributed.Object_Key := + "i";

   Conf  : constant String :=
   "<sancta>" &
   "   <agent id=""Ari"">" &
   "      <udp address=""127.0.0.1"" port=""5001""/>" &
   "      <channel name=""database"" subscribe=""true""/>" &
   "   </agent>" &
   "   <agent id=""Ben"">" &
   "      <udp address=""127.0.0.1"" port=""5002""/>" &
   "      <channel name=""database"" subscribe=""true""/>" &
   "   </agent>" &
   "   <agent id=""Ced"">" &
   "      <udp address=""127.0.0.1"" port=""5003""/>" &
   "      <channel name=""database"" subscribe=""true""/>" &
   "   </agent>" &
   "   <agent id=""Dan"">" &
   "      <udp address=""127.0.0.1"" port=""5004""/>" &
   "      <channel name=""database"" subscribe=""true""/>" &
   "   </agent>" &
   "</sancta>";

   C     : constant Xml.Node := Xml.From_String (Conf);

   Prev : Integer := 0;
   type Test_Listener is new Distributed.Key_Listener with null record;
   procedure On_Key_Stored (This  : in out Test_Listener;
                            From  : in     Node_Id;
                            Key   : in     Distributed.Object_Key;
                            Value : in     Distributed.Object_Data'Class;
                            Meta  : in     Distributed.Object_Metadata)
   is
      pragma Unreferenced (This);
      M : Distributed.Object_Metadata renames Meta;
   begin
--      Log ("[LISTEN] " & External_Tag (Value'Tag), Always);
      Log ("[LISTEN] " & Distributed.Image (Key) & "=" & Int (Value).I'Img &
           "; Fr: " & Network.Image (From) &
           "; Ow: " & Network.Image (M.Owner) &
           "; Key: " & Distributed.Image (Key, M), Always);
      if Int (Value).I /= Prev + 1 then
         Log ("[LISTEN] ***************** Sequence error ***************", Always);
      end if;
      Prev := Int (Value).I;
   end On_Key_Stored;

   Tl : aliased Test_Listener;

begin
   Set_Decorator (Utils.Prepend_Timestamp'Access);
   Set_Level (Debug);
--   Enable_Section (Network.Layer.Udp.Log_Section);
   Enable_Section (Network.Groups.Log_Section);
--     Enable_Section (Distributed.Datastore.Log_Section);
--     Enable_Section (Distributed.Datastore.Detail_Section);

   Network.Groups.Init (C);
   Link.Init (C);

   Log ("Starting...", Always);

   Store.Listen (Ki, Tl'Unchecked_Access);

   Log ("Requesting I...", Always);
   Store.Request (Ki); -- Get value in case someone has it
   delay 1.0;

   if not Store.Contains (Ki) then
      Log ("Storing I not received", Always);
      Store.Set (ki, I);
   end if;

   loop
--      delay Duration (5.0 + Random.Uniform * 10.0);
      delay 0.1;

      declare
         Latency : Chronos.Object;
         procedure Add (Key : in     Distributed.Object_Key;
                        Val : in out Distributed.Object_Data'Class;
                        Met : in     Distributed.Object_Metadata)
         is
            pragma Unreferenced (Key, Met);
            I : Int renames Int (Val);
         begin
            Log ("[LATENCY] " & Latency.Image, Always);
            I.I := I.I + 1;
         end Add;

         Success : Boolean;
      begin
         if Network.Image (Id) = "Ari" or True then
            Store.Update (Ki, Add'Access, Success, 1.0);
         end if;
         if not Success then
            Log ("[TIMEOUT] Latency: " & Latency.Image, Always);
         end if;
      end;

--        I := Int (Store.Get (Ki));

--        if Network.Image (Id) = "Ari" or True then
--           I.I := I.I + 1;
--           Store.Set (Ki, I);
--        end if;
   end loop;
exception
   when E : others =>
      Log ("Main: " & Report (E), Error);
end Sancta.Main.Distributed_Test;
