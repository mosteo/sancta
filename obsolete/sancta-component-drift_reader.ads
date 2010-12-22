with Sancta.Component.Root;

with Agpl.Chronos;
with Agpl.Xml;

with Ada.Calendar;

--  Player component.
--  Provides the gps_0 interface from player

--  This component accepts a "base_pose" attribute, which is where
--  we place the Origin for the meters reported by the GPS.
--  This is necessary since the large values given in meters overflow somewhere else.

package Sancta.Component.Drift_Reader is

   pragma Elaborate_Body;

   Plugin_Name : constant String := "drift_reader";

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Agpl.Protected_Datastore.Object_Key;
                         Value : in     Agpl.Protected_Datastore.Object_Data'Class);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with
   record
      Use_Truth  : Boolean;
      Timer_Curr : aliased Agpl.Chronos.Object;
      Timer_Odom : aliased Agpl.Chronos.Object;
      Period     : Duration := 1.0;
   end record;

end Sancta.Component.Drift_Reader;
