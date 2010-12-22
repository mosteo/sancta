with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Component.Player_Safe;

with Agpl; with Agpl; use Agpl;

with Ada.Text_Io; use Ada.Text_Io;

pragma Elaborate_All (Sancta.Component.Factory);

package body Sancta.Component.Drift_Reader is

   type Object_Access is access all Object'Class;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Period := Duration'Value
        (Xml.Get_Attribute (Config, "period", "1.0"));

      This.Use_Truth := Boolean'Value (Xml.Get_Attribute
                                         (Config, "use_truth", "False"));

      if This.Use_Truth then
         Player_Safe.Object.Connect_Simulation_0;
      end if;

      --  Add ourselves as listeners:
      if This.Use_Truth then
         This.Subscribe ("odom_pose");
         This.Subscribe ("current_pose");
      end if;

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Agpl.Protected_Datastore.Object_Key;
                         Value : in     Agpl.Protected_Datastore.Object_Data'Class)
   is
      Timo : Chronos.Object_Access;
      use Types;
   begin
      if Key = "odom_pose" then
         Timo := This.Timer_Odom'Unrestricted_Access;
      elsif Key = "current_pose" then
         Timo := This.Timer_Curr'Unrestricted_Access;
      end if;

      if Timo.Elapsed > This.Period then
         Timo.Reset;
         declare
            Truth_Pose : Types.Pose renames
                           Player_Safe.Object.Get_Pose ("Ari");
         begin
            Put_Line ("Drift for " & Key & " is " &
                      To_String (Datastore.Pose (Value).Pose - Truth_Pose));
         end;
      end if;
   end Key_Stored;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
      use Types;
   begin
      if not This.Use_Truth then
         Next := Clock + This.Period;
      else
         Next := Clock + 100.0;
      end if;

      if not This.Use_Truth then
         declare
            Odom_Pose : constant Types.Pose := Datastore.Get_Pose ("odom_pose");
            Curr_Pose : constant Types.Pose := Datastore.Get_Pose ("current_pose");
         begin
            Put_Line ("Drift from odometry is " &
                      To_String (Curr_Pose - Odom_Pose));
         end;
      end if;
   exception
      when Agpl.Protected_Datastore.Data_Not_Present =>
         null;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Drift_Reader;
