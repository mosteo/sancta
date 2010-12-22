with Sancta.Config;
with Sancta.Datastore;
with Sancta.Datastore.Types;
with Sancta.Debug2;
with Sancta.Network.Consumer.Root;
with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Component.Factory; pragma Elaborate_All (Sancta.Component.Factory);
with Sancta.Component.Task_Manager;
with Sancta.Robot_Actions;
with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Speed_Driving;
with Sancta.Tasks.Wait_For_Orders;
with Sancta.Types.Operations;

with Sancta.Tasks.Containers;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

with Gnat.Os_Lib;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Component.Executor is

   use Ada.Calendar;
   package Task_Lists renames Sancta.Tasks.Containers.Lists;

   type Object_Access is access all Object;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Link :=
        Network.Layer.Object_Access
          (Datastore.Network_Layer
               (Datastore.Object.Get (Link_Key)).Ref);

      Network.Consumer.Root.Subscribe
        (This.Inbox'Access, This.Link, Network.Groups.Emergency_Channel);

      return Component.Object_Access (This);
   end Create;

   ---------------------------------
   -- Do_Explore_Directed_Segment --
   ---------------------------------

   procedure Do_Explore_Directed_Segment (This : in out Object;
                                          Job  : in out Sancta.Tasks.Object'Class;
                                          Done : in out Boolean)
   is
      T : Tasks.Explore_Directed_Segment.Object renames
        Tasks.Explore_Directed_Segment.Object (Job);

      Goal : Types.Pose;
      use Types.Operations;
   begin
      --  First, faultly implementation, will go to the start and then to the
      --  end of the segment, with obstacle avoidance always on.
      pragma Incomplete ("When walking the segment we shouldn't deviate from it");

      if T.On_Segment then
         Goal := T.Get_To;
      else
         Goal := T.Get_From;
      end if;

      declare
         Go : Tasks.Goto_Pose.Object'Class :=
                Tasks.Goto_Pose.Create (Goal,
                                        Use_Angle  => True,
                                        Margin_D   =>
                                          Types.Real'Value
                                            (Xml.Get_Attribute
                                                 (Config.Node_Options,
                                                  "goal_dist",
                                                  "0.5")));
         Go_Done : Boolean := False;
      begin
         This.Do_Goto_Pose (Go, Go_Done);

         if Go_Done then
            if T.On_Segment then
               Done := True;
            else
               T.Set_On_Segment;
            end if;
         end if;
      end;
   end Do_Explore_Directed_Segment;

   ------------------
   -- Do_Goto_Pose --
   ------------------

   procedure Do_Goto_Pose (This : in out Object;
                           Job  : in out Sancta.Tasks.Object'Class;
                           Done : in out Boolean)
   is
      T            : Tasks.Goto_Pose.Object renames Tasks.Goto_Pose.Object (Job);
      Goal         : constant Types.Pose := T.Pose;
      Current_Pose : constant Types.Pose :=
                       Datastore.Get_Pose (This.Key (Requires_World_Pose));

      use Types;
      use Types.Operations;
      use Debug2;
   begin
      Log ("Executing Goto_Pose " & To_String (T.Pose),
           Debug, Detail_Section);
      --  Check finished
      if Distance (Goal, Current_Pose) <= T.Margin_Dist and then
        ((not T.Use_Angle) or else
           abs (Goal.A - Current_Pose.A) <= T.Margin_Angle)
      then
         Done := True;
      else
         if not T.Use_Angle then
            Datastore.Object.Set
              (This.Key (Provides_Action),
               Datastore.Types.Robot_Action'
                 (Robot_Action => (Robot_Actions.Goto_Pose_No_Angle, Goal)));
         else
            Datastore.Object.Set
              (This.Key (Provides_Action),
               Datastore.Types.Robot_Action'
                 (Robot_Action => (Robot_Actions.Goto_Pose, Goal)));
         end if;

--           if Distance (T.Pose, Current_Pose) <= T.Margin_Dist then
--              Goal.X := Current_Pose.X;
--              Goal.Y := Current_Pose.Y;
--           end if;
      end if;
   exception
      when E : others =>
         Log ("Robot.Do_Goto_Pose: " & Report (E), Warning);
         Log ("T Pose is "   & To_String (T.Pose), Warning);
         Log ("Bot pose is " & To_String (Current_Pose), Warning);
   end Do_Goto_Pose;

   ----------------------
   -- Do_Speed_Driving --
   ----------------------

   procedure Do_Speed_Driving (This : in out Object;
                                          Job  : in out Sancta.Tasks.Object'Class;
                                          Done : in out Boolean)
   is
      T : Tasks.Speed_Driving.Object renames Tasks.Speed_Driving.Object (Job);
   begin
      if not T.Is_Started then
         T.Mark_Started;
      end if;

      --  Check finished
      if T.Finished then
         Done := True;
      else
         Datastore.Object.Set
           (This.Key (Provides_Action),
            Datastore.Types.Robot_Action'
              (Robot_Action => (Robot_Actions.Set_Velocity, T.Velocity)));
      end if;
   end Do_Speed_Driving;

   ------------------------
   -- Do_Wait_For_Orders --
   ------------------------

   procedure Do_Wait_For_Orders (This : in out Object;
                                 Job  : in out Sancta.Tasks.Object'Class;
                                 Done : in out Boolean)
   is
   begin
      Datastore.Object.Set
        (This.Key (Provides_Action),
         Datastore.Types.Robot_Action'
           (Robot_Action => (Robot_Actions.Set_Velocity, Types.Null_Pose)));
      Done := False;
   exception
      when E : others =>
         Log ("Robot.Do [" & External_Tag (Job'Tag) & "]: " & Report (E), Warning);
   end Do_Wait_For_Orders;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : in out Object) is
      Wait : Tasks.Wait_For_Orders.Object;
      Done : Boolean := False;
   begin
      if Datastore.Object.Contains (This.Key (Requires_Task_List)) then
         declare
            Tasks : Task_Lists.List :=
                      Component.Task_Manager.Shared_Task_List
                        (Datastore.Object.Get (This.Key (Requires_Task_List))).List;
         begin
            if Tasks.Is_Empty then
               Do_Wait_For_Orders (This, Wait, Done);
            else
               declare
                  T    :          Sancta.Tasks.Object'Class := Tasks.First_Element;
                  Told : constant Sancta.Tasks.Object'Class := T;
                  use type Sancta.Tasks.Object'Class;
               begin
                  if T in Sancta.Tasks.Explore_Directed_Segment.Object'Class then
                     Do_Explore_Directed_Segment (This, T, Done);
                  elsif T in Sancta.Tasks.Goto_Pose.Object'Class then
                     Do_Goto_Pose (This, T, Done);
                  elsif T in Sancta.Tasks.Speed_Driving.Object'Class then
                     Do_Speed_Driving (This, T, Done);
                  elsif T in Sancta.Tasks.Wait_For_Orders.Object'Class then
                     Do_Wait_For_Orders (This, Wait, Done);
                  else
                     Log ("Unknown task: " & T.To_String, Warning, Log_Section);
                     Do_Wait_For_Orders (This, Wait, Done);
                  end if;

                  if Done then
                     Tasks.Delete_First;
                     Datastore.Object.Set
                       (This.Key (Requires_Task_List),
                        Component.Task_Manager.Shared_Task_List'(List => Tasks));
                  elsif Told /= T then
                     Tasks.Delete_First;
                     Tasks.Prepend (T);
                     Datastore.Object.Set
                       (This.Key (Requires_Task_List),
                        Component.Task_Manager.Shared_Task_List'(List => Tasks));
                  end if;
               end;
            end if;
         end;
      else
         Do_Wait_For_Orders (This, Wait, Done);
      end if;
   end Execute;

   ----------------------
   -- Process_Messages --
   ----------------------

   procedure Process_Messages (This : in out Object) is
   begin
      while not This.Inbox.Is_Empty loop
         declare
            M : constant Network.Message'Class := This.Inbox.Get_First;
         begin
            This.Inbox.Remove_First;

            if M in Network.Messages.Shutdown_Type then
               -- SHUTDOWN --

               declare
                  task type Exitor;
                  task body Exitor is
                  begin
                     delay 1.0;
                     Gnat.Os_Lib.Os_Exit (0);
                  end Exitor;
                  T : constant access Exitor := new Exitor;
                  pragma Unreferenced (T);
               begin
                  null;
                  --  Damn bad trick to delay shutdown without stopping
                  --  the plugins.
               end;

               Log ("Executor: SANCTA node exiting [Shutdown msg]",
                    Always, Log_Section);
            else
               null;
            end if;
         end;
      end loop;
   end Process_Messages;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time) is
   begin
      Process_Messages (This);

      This.Execute;

      Next := Clock + 0.01;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Executor;
