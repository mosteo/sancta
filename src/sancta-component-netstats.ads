with Agpl.Chronos;
with Agpl.Tasking.Period;
with Sancta.Component.Root;
with Sancta.Network.Layer.Root;

package Sancta.Component.Netstats is

--  This component provides several network related stats.
--  Currently, roundtrip (ping) time and average BW in/out per second.
--  Ping requires a remote redirect_listener to get ACKs from.
--  Period is hardcoded to 1.0

   Log_Section : constant String := "sancta.component.netstats";

   Name : aliased constant Component_Name := "netstats";

   Requires_Link  : constant Internal_Key := "link";
   Provides_Stats : constant Internal_Key := "stats";

   Opt_Enable_Stdout : constant Option_Attr := "enable_stdout";
   Def_Enable_Stdout : constant Boolean     := False;
   --  Log to stdout if present

   Opt_Enable_Ping : constant Option_Attr  := "enable_ping"; -- bool
   Def_Enable_Ping : constant Boolean      := True;

   --  Only needed if ping is enabled:
   Opt_Ping_Dest    : constant Option_Attr  := "dest"; -- some node_id
   Opt_Ping_Channel : constant Option_Attr  := "channel";

   Ping_Key : constant External_Key := "_nsp";
   --  This key is used for pinging; shouldn't be used anywere else

   procedure Register;

   type Stats is
     new Data
   with record
      Bw_In     : Float;
      Bw_Out    : Float;
      With_Ping : Boolean;
      Roundtrip : Duration;
   end record;
   pragma Pack (Stats);

   function To_String (S : Stats) return String;

private

   protected type Safe_Object is

      procedure Reset;
      procedure ACKd;
      function Elapsed return Duration;

   private

      Timer : Agpl.Chronos.Object;
      Rtrip : Duration := 0.0;

   end Safe_Object;

   type Object is new Root.Object with record
      Link      : Network.Layer.Root.Object_Access;
      With_Ping : Boolean;
      Chan      : Network.Channel;
      Dest      : Node_Id;

      Safe      : Safe_Object;

      Period : Agpl.Tasking.Period.Object := Agpl.Tasking.Period.Create (1.0);
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Netstats;
