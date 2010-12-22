with Sancta.Datastore;
--  with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Component.Factory;
with Sancta.Component.Player_Safe;

pragma Elaborate_All (Sancta.Component.Factory);

--  with Agpl.Trace; use Agpl.Trace;

--  with Ada.Text_Io; use Ada.Text_Io;

--  Player component.
--  Provides the Position2d_0 interface from player
--  Sends commands also to that interface.
--  Because of that, the goal pose should be in the same reference that the
--  player interface is using.

--  Example: if position2d:0 is VFH reading from position2d:1 (odometry),
--  we should give the goal pose in the position2d:1 reference
--  (which in this case is the same as position2d:0).

package body Sancta.Component.Move is

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node) return Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.As_Emergency_Stop := new String'(This.Key (Requires_Emergency_Stop));
      This.As_Goal_Action    := new String'(This.Key (Requires_Goal_Action));

      Datastore.Object.Set (This.As_Goal_Action.all,
                            Goal_Action'(Kind => Set_Vel,
                                         Vel  => (0.0, 0.0, 0.0)));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      Next := Clock + 0.1;
      --  Should we stop?
      if Datastore.Object.Contains (This.As_Emergency_Stop.all) then
         Player_Safe.Object.Set_Vel (0.0, 0.0, 0.0);
      else
         --  Check the goal action and perform
         declare
            Goal : constant Goal_Action := Goal_Action
              (Datastore.Object.Get (This.As_Goal_Action.all));
         begin
            case Goal.Kind is
               when Set_Goal =>
                  Player_Safe.Object.Goto_Pose (Goal.Goal);
               when Set_Vel =>
                  Player_Safe.Object.Set_Vel (Goal.Vel.X,
                                              Goal.Vel.Y,
                                              Types.Real (Goal.Vel.A));
            end case;
         end;
      end if;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Move;
