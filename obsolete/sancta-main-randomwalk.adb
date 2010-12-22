with Sancta.Auctioner;
with Sancta.Config;
with Sancta.Network;
with Sancta.Network.Layer;
with Sancta.Network.Layer.Udp;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;        use Agpl.Trace;
with Agpl.Xml;
with Agpl; use Agpl;

with Gnat.Os_Lib;

with Ada.Calendar;              use Ada.Calendar;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Text_Io; use Text_Io;

--  MAIN CONTROL CONSOLE

procedure Sancta.Main.Randomwalk is

   Our_Id : aliased Network.Node_Id := Network.Value ("Xyd");
   --  Zen is the node id for the control station.

   Link   : aliased Network.Layer.Udp.Object (Our_Id'Access);
   Seller : Auctioner.Object (Link'Access);

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
      Put_Line (Program_Name & " --config <file> [OPTIONS]");
      Put_Line ("   No options defined for now.");
   end Usage;

   Period : Duration;
   Xrange : Types.Real;
   Yrange : Types.Real;

   Timer : Time := Clock - 999.0;
   Seed  : Generator;

begin

   if not Exists ("--config") then
      Usage;
      return;
   end if;

   --  Common configuration.
   Config.Init (Our_Id, Debug);
   Enable_Section (Config.Tracer, Auctioner.Log_Section);
--   Enable_Section (Config.Tracer, Network.Layer.Udp.Log_Section);

   Period := Duration'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "period"));

   Xrange := Types.Real'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "xrange"));

   Yrange := Types.Real'Value
     (Xml.Get_Attribute
        (Xml.Get ("parameters", Config.Node_Options),
         "yrange"));

   Reset (Seed);

   --  Set up comms.
   Link.Init (Config.Options);

   Seller.Set_Configuration (Config.Options);

   --  Main loop
   loop
      delay 0.01;
      Seller.Run;

      --  Create alert
      if Clock - Timer >= Period then
         Timer := Clock;
         declare
            use Sancta.Types;
            Job : Tasks.Goto_Pose.Object;
         begin
            Job.Set_Pose ((Real (Random (Seed) * 2.0 - 1.0) * Xrange,
                           Real (Random (Seed) * 2.0 - 1.0) * Yrange,
                           Angle (Random (Seed) * 2.0 - 1.0) * 3.1415926535));
            Seller.Add_Task (Job, Lasts => 1.0);
         end;
      end if;
   end loop;

exception
   when E : others =>
      Log ("Randomwalk [Main]: " & Report (E), Error);
      Shutdown;
end Sancta.Main.Randomwalk;
