with Sancta.Convert; use Sancta.Convert;
with Sancta.Datastore;
with Sancta.Datastore.Types;
with Sancta.Debug2;
with Sancta.Component.Factory; pragma Elaborate_All (Sancta.Component.Factory);
with Sancta.Component.Player_Client;
with Sancta.Robot_Actions;
with Sancta.Types.Transformations;

with Agpl.Trace; use Agpl.Trace;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Component.Player_Executor is

   use Ada.Calendar;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      Component.Player_Client.Create_And_Subscribe (This.Pos, 1);

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
   begin
      if Datastore.Object.Contains (Action_Key) then
         declare
            Action : constant Robot_Actions.Object :=
                       Datastore.Types.Robot_Action
                         (Datastore.Object.Get (Action_Key)).Robot_Action;
         begin
            case Action.Kind is
               when Robot_Actions.Goto_Pose | Robot_Actions.Goto_Pose_No_Angle =>
                  declare
                     use type Robot_Actions.Kinds;
                     Odom_Pose  : constant Types.Pose := +This.Pos.Get_Pose;
                     World_Pose : constant Types.Pose :=
                                      Datastore.Get_Pose (World_Pose_Key);
                     Odom_Goal  : Types.Pose :=
                                      Types.Transformations.World_To_Odom
                                        (Odometry_Pose => Odom_Pose,
                                         World_Pose    => World_Pose,
                                         Goal_Pose     => Action.World_Goal);
                     use Debug2;
                  begin
                     Log ("Odom Pose: " & To_String (Odom_Pose), Debug, Log_Section);
                     Log ("Odom Goal: " & To_String (Odom_Goal), Debug, Log_Section);
                     Log ("WorldPose: " & To_String (World_Pose), Debug, Log_Section);
                     Log ("WorldGoal: " & To_String (Action.World_Goal), Debug, Log_Section);

                     if Action.Kind = Robot_Actions.Goto_Pose_No_Angle then
                        Odom_Goal.A := Odom_Pose.A;
                     end if;
                     This.Pos.Set_Cmd_Pose (+Odom_Goal.X,
                                            +Odom_Goal.Y,
                                            +Odom_Goal.A);
                  end;
               when Robot_Actions.Set_Velocity =>
                  This.Pos.Set_Cmd_Vel (+Action.Velocity.X,
                                        +Action.Velocity.Y,
                                        +Action.Velocity.A);
            end case;
         end;
      else
         This.Pos.Set_Cmd_Vel (0.0, 0.0, 0.0);
      end if;

      Next := Clock + 0.01;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Player_Executor;
