with Sancta.Component.Root;

with Agpl.Xml;

with Ada.Calendar;

--  Player component.
--  Provides the Position2d_0 interface from player
--  Sends commands also to that interface.
--  Because of that, the goal pose should be in the same reference that the
--  player interface is using.

--  Example: if position2d:0 is VFH reading from position2d:1 (odometry),
--  we should give the goal pose in the position2d:1 reference
--  (which in this case is the same as position2d:0).

package Sancta.Component.Move is

   pragma Elaborate_Body;

   Plugin_Name : constant String := "move";

   Requires_Goal_Action    : Internal_Key renames Data_Goal_Action;
   Requires_Emergency_Stop : Internal_Key renames Data_Emergency_Stop;

   type Object is new Root.Object with private;

   function Create (Config : in Agpl.Xml.Node) return Object_Access;

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      --  Convenience fields:
      As_Emergency_Stop,
      As_Goal_Action : access String;
   end record;

end Sancta.Component.Move;
