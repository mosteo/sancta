with Sancta.Auctioner;
with Sancta.Config;
with Sancta.Datastore;
with Sancta.Gui.Robot_Network_Update;
with Sancta.Network;
with Sancta.Network.Groups;
with Sancta.Network.Layer.Udp;
with Sancta.Robot;
with Sancta.Traderbot;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;
with Agpl.Xml;          use Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Text_Io; use Text_Io;

procedure Sancta.Main.Traderbot is
begin
   declare
      Our_Id : aliased Network.Node_Id;

      Link  : aliased Network.Layer.Udp.Object (Our_Id'Access);
      Bot   : aliased Robot.Object;
      Relay :         Gui.Robot_Network_Update.Object (Link'Unchecked_Access);

      Trader : Sancta.Traderbot.Object (Bot'Access,
                                        Link'Access);

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         --  Orderly shut down:
         Link.Shutdown;

         --  Finally, forced shut down:
         delay 1.0;
         Gnat.Os_Lib.Os_Exit (-1);
      exception
         when E : others =>
            Log ("Shutting down: " & Report (E), Warning);
            Gnat.Os_Lib.Os_Exit (-1);
      end Shutdown;

      -----------
      -- Usage --
      -----------

      procedure Usage is
      begin
         Put_Line ("Usage:");
         Put_Line (Program_Name & " --id <node_id> --config <file> [OPTIONS]");
         Put_Line ("   No options defined for now.");
      end Usage;

      Time_Policy : Sancta.Traderbot.Auction_Time_Policies;

   begin
      if not Exists ("--id") or else not Exists ("--config") then
         Usage;
         Shutdown;
         return;
      end if;

      --  Common configuration.
      Config.Init (Link => Link'Unchecked_Access, Log_Level => Debug);
      --  Config.Init (Log_Level => Debug);

      Our_Id := Config.Node_Id;

      --  Set up comms.
      Network.Groups.Init (Config.Options);
      Link.Init (Config.Options);

      Log ("Starting...", Always);

      --  Set up robot.
      Bot := Sancta.Robot.Create (Network.Image (Config.Node_Id));
      Bot.Add_Listener2 (Relay);

      --  Store references to robot and network layer for plugins access:
      Datastore.Object.Set
        ("robot",
         Datastore.Robot'
           (Datastore.Object_Data with Ref => Bot'Unchecked_Access));
      Datastore.Object.Set
        ("network_layer",
         Datastore.Network_Layer'
           (Datastore.Object_Data with Ref => Link'Unchecked_Access));

      Time_Policy.Periodic := 5.0;
      Trader.Set_Configuration
        (Config.Options,
         Cost_Policy         => Auctioner.Cost_Policies'Value
           (Xml.Get_Attribute ("traderbot", "cost_policy",
            Config.Options, "full_cost")),
         Auction_Time_Policy => Time_Policy
        );

      Log ("Traderbot ready", Informative);

      --  Main loop
      declare
         Done : Boolean := False;
      begin
         while not Done loop
            delay 0.01;

            Trader.Run (Done);

         end loop;
      end;

      Bot.Emergency_Stop;
      Shutdown;
   end;

exception
   when E : others =>
      Put_Line ("Traderbot [Main]: " & Report (E));
      Log ("Traderbot [Main]: " & Report (E), Error);
      --      Shutdown;
end Sancta.Main.Traderbot;
