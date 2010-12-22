with Sancta.Config;
with Sancta.Cost_Utils;
with Sancta.Datastore;
with Sancta.Debug2; use Sancta.Debug2;
with Sancta.Gui.Robot_Data;
with Sancta.Network;
with Sancta.Component;

with Sancta.Tasks.Explore_Directed_Segment;
with Sancta.Tasks.Goto_Pose;
with Sancta.Tasks.Positioned;
with Sancta.Tasks.Speed_Driving;
with Sancta.Tasks.Wait_For_Orders;
with Sancta.Tasks.Wander;
with Sancta.Types.Operations;
with Sancta.Types.Real_Math;
with Sancta.Types.Transformations;

with Sancta.Agent;
with Sancta.Tasks.Handle;
with Sancta.Tasks.Still;
with Agpl.Monitor;
with Agpl.Protected_Datastore;
with Agpl.Xml;
with Agpl.Trace; use Agpl.Trace;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Robot is

   Sem : aliased Monitor.Semaphore;

   package Math renames Sancta.Types.Real_Math;

   Null_Pose : constant Types.Pose := (0.0, 0.0, 0.0);

   use type Gui.Robot_Data.Handle.Object;
   use type Types.Pose;
   use type Types.Real;
   use type Types.Angle;

   procedure Check_Gui_Tasks (This : in out Object);
   procedure Set_Gui_Tasks (This : in out Object);
   procedure Do_Goto_Pose (This : in out Sancta.Agent.Object'Class;
                           Job  : in out Sancta.Tasks.Object'Class;
                           Done : in out Boolean);
   procedure Update_Gui_Command (This : in out Object; Command : String);
   procedure Update_Gui_Current_Task
     (This : in out Object; Job  : in Sancta.Tasks.Object'Class);
   procedure Update_Gui_Pose (This : in out Object);


   ------------------
   -- Add_Listener --
   ------------------

   procedure Add_Listener (This : in out Object;
                           X    : in     Gui.Robot_Data.Messages.Object'Class)
   is
   begin
      Gui.Robot_Data.Messages.Add_Listener (This.Gui_Manager, X);
   end Add_Listener;

   procedure Add_Listener2 (This : in out Object;
                            X    : in     Gui.Robot_Data.Messages2.Object'Class)
   is
   begin
      Gui.Robot_Data.Messages2.Add_Listener (This.Gui_Manager2, X);
   end Add_Listener2;

   ------------
   -- Create --
   ------------

   function Create (Name : in String) return Object is
      This : Object;
   begin
      Set_Name (This, Name);

      declare
         use Gui.Robot_Data.Handle;
      begin
         Bind (This.Gui_Data, new Gui.Robot_Data.Object);
      end;

      This.Register_Executers;

      Config.Create_Plugins;

      return This;
   end Create;

   ---------------------------------
   -- Do_Explore_Directed_Segment --
   ---------------------------------

   procedure Do_Explore_Directed_Segment (This : in out Sancta.Agent.Object'Class;
                                          Job  : in out Sancta.Tasks.Object'Class;
                                          Done : in out Boolean)
   is
      Bot  : Object renames Object (This);
      T    : Tasks.Explore_Directed_Segment.Object renames
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
                                        Use_Angle   => True,
                                        Margin_D =>
                                          Types.Real'Value
                                            (Xml.Get_Attribute
                                                 (Config.Node_Options,
                                                  "goal_dist",
                                                  "0.5")));
         Go_Done : Boolean := False;
      begin
         Do_Goto_Pose (This, Go, Go_Done);

         Update_Gui_Current_Task (Bot, T);
         Update_Gui_Command (Bot, "Go To Pose " & To_String (Goal));

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

   procedure Do_Goto_Pose (This : in out Sancta.Agent.Object'Class;
                           Job  : in out Sancta.Tasks.Object'Class;
                           Done : in out Boolean)
   is
      Bot  : Object renames Object (This);
      T    : Tasks.Goto_Pose.Object renames
        Tasks.Goto_Pose.Object (Job);

      Current_Pose : constant Types.Pose := Bot.Get_Pose;
      Goal         :          Types.Pose := T.Pose;

      use Types.Operations;
   begin
      Log ("Executing Goto_Pose " & To_String (T.Pose),
           Debug, Detail_Section);
      --  Check finished
      if Distance (T.Pose, Current_Pose) <= T.Margin_Dist and then
        ((not T.Use_Angle) or else
           abs (T.Pose.A - Current_Pose.A) <= T.Margin_Angle)
      then
         Done := True;
      else
         if not T.Use_Angle then
            Goal.A := Current_Pose.A;
         end if;

         if Distance (T.Pose, Current_Pose) <= T.Margin_Dist then
            Goal.X := Current_Pose.X;
            Goal.Y := Current_Pose.Y;
         end if;

         Goto_Pose (Bot, Goal);
      end if;
   exception
      when E : others =>
         Log ("Robot.Do_Goto_Pose: " & Report (E), Warning, Log_Section);
         Log ("T Pose is " & To_String (T.Pose), Warning, Log_Section);
         Log ("Bot pose is " & To_String (Current_Pose), Warning, Log_Section);
   end Do_Goto_Pose;

   ------------------
   -- Do_Hold_Pose --
   ------------------

--     procedure Do_Hold_Pose
--       (This     : in out Object;
--        T        : in out Tasks.Hold_Pose.Object;
--        Done     : in out Boolean)
--     is
--        pragma Unreferenced (This);
--        use Tasks.Types;
--        use Smart_Cover;
--        Data : Tasks.Types.Cover_Gaps_And_Dispatch renames Val (T.Get_Data);
--     begin
--        Done := Data.Pending = 0;
--     end Do_Hold_Pose;

   -----------------------
   -- Do_Pursuit_Common --
   -----------------------
   --  Common info updating for pursuit related tasks.
--     procedure Do_Pursuit_Common
--       (This : in out Object;
--        Info : in out Tasks.Types.Pursuit_Info) is
--     begin
--        Info.Poses.Include (This.Get_Name,
--                            Get_Pose (This));
--        Info.Redraw;
--     end Do_Pursuit_Common;

   ----------------------
   -- Do_Speed_Driving --
   ----------------------

   procedure Do_Speed_Driving (This : in out Sancta.Agent.Object'Class;
                               Job  : in out Sancta.Tasks.Object'Class;
                               Done : in out Boolean)
   is
      Bot  : Object renames Object (This);
      T    : Tasks.Speed_Driving.Object renames
        Tasks.Speed_Driving.Object (Job);
   begin
      if not T.Is_Started then
         T.Mark_Started;
      end if;

      --  Check finished
      if T.Finished then
         Done := True;
      else
         Bot.Set_Vel (T.Velocity.X,
                      T.Velocity.Y,
                      Types.Real (T.Velocity.A));
      end if;
   end Do_Speed_Driving;

   -------------------------
   -- Do_Track_Two_Points --
   -------------------------

--     procedure Do_Track_Two_Points
--       (This : in out Object;
--        T    : in out Tasks.Track_Two_Points.Object;
--        Done : in out Boolean)
--     is
--        use Types.Operations;
--        P : constant Types.Pose := This.Get_Pose;
--        G : constant Types.Pose := T.Get_Goal;
--     begin
--        if not T.Has_Goal then
--           T.Compute_Goal (3.0); -- Arbitrary FOV for now.
--           Update_Gui_Command (This, "Go To Goal (Tracking two points)");
--        else
--           --  Check finished: near destination and no velocities
--           if Distance (P, G) <= 1.0 and then
--              Get_Vel (This) = Null_Pose
--           then
--              if T.Has_Notified then
--                 Done := True;
--              else
--                 T.Notify_Holder;
--              end if;
--           else
--              --  Go there.
--              Goto_Pose (This, G);
--           end if;
--        end if;
--     end Do_Track_Two_Points;

   procedure Do_Wait_For_Orders (This : in out Sancta.Agent.Object'Class;
                                 Job  : in out Sancta.Tasks.Object'Class;
                                 Done : in out Boolean)
   is
      Bot  : Object renames Object (This);
   begin
      Bot.Set_Vel (0.0, 0.0, 0.0);
      Done := False;
   exception
      when E : others =>
         Log ("Robot.Do [" & External_Tag (Job'Tag) & "]: " & Report (E),
              Warning, Log_Section);
   end Do_Wait_For_Orders;

   ---------------
   -- Do_Wander --
   ---------------

   procedure Do_Wander
     (This     : in out Object;
      The_Task : in out Tasks.Wander.Object)
   is
      use type Types.Angle;
      use type Types.Real;
      Size : constant Types.Pose := Get_Size (This);
      Look : constant Types.Angle :=
               Types.Angle
                 (Math.Arctan (Size.X / 2.0 + The_Task.Safe_Side,
                               Size.Y / 2.0 + The_Task.Safe_Front));
      Hazard : constant Types.Range_Reading :=
                 Get_Closest_Obstacle (This, - Look, Look);
   begin
      if Hazard.D <= The_Task.Safe_Front + Size.X / 2.0 then
         --  Rotate until out of risk.
         if This.Status /= Wander_Rotate then
            Update_Gui_Command (This, "Wander: Rotating");
            This.Status := Wander_Rotate;
            if Hazard.A >= 0.0 then
               Set_Vel (This, 0.0, 0.0, -The_Task.Rot_Speed);
            else
               Set_Vel (This, 0.0, 0.0,  The_Task.Rot_Speed);
            end if;
         end if;
      elsif Hazard.D > The_Task.Safe_Front * 1.2 + Size.X / 2.0 then -- some hysteresis.
         if This.Status /= Wander_Forward then
            Update_Gui_Command (This, "Wander: Going forward");
            This.Status := Wander_Forward;
            --  Go ahead.
            Set_Vel (This, The_Task.Speed, 0.0, 0.0);
         end if;
      elsif This.Status /= Wander_Rotate and then This.Status /= Wander_Forward then
         Update_Gui_Command (This, "Wander: Rotating");
         This.Status := Wander_Rotate;
         if Hazard.A >= 0.0 then
            Set_Vel (This, 0.0, 0.0, - The_Task.Rot_Speed);
         else
            Set_Vel (This, 0.0, 0.0,  The_Task.Rot_Speed);
         end if;
      end if;
   end Do_Wander;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This     : in out Object;
      The_Task : in out Sancta.Tasks.Primitive.Object'Class;
      Plan     : in out Sancta.Plan.Object;
      Done     :    out Boolean)
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);

      pragma Unreferenced (Plan);
      use Types.Operations;
      use Types.Transformations;

      use type Sancta.Tasks.Primitive.Object'Class;
      Original : constant Sancta.Tasks.Primitive.Object'Class := The_Task;
   begin
      if not Is_Connected (This) then
         return;
         --  raise Connection_With_Robot_Lost;
      end if;

      Done           := False;
      This.Executing := True;

      Update_Gui_Pose (This);
      Check_Gui_Tasks (This);

      --  Positioning tasks
      if The_Task in Tasks.Positioned.Object'Class and then
        not (The_Task in Tasks.Goto_Pose.Object)
      then
         declare
            T : Tasks.Positioned.Object renames
              Tasks.Positioned.Object (The_Task);
            P : constant Types.Pose := Get_Pose (This);
            use type Types.Real;
            use type Types.Angle;
         begin
            --  Check finished
            if Distance (T.Pose, P) <= 1.0 and then
              This.Get_Vel = Null_Pose
            then
               Done := True;
            else
               Update_Gui_Command (This,
                 "Go To Pose " &
                 To_String (Tasks.Positioned.Object
                   (The_Task).Pose));
               Goto_Pose (This, Tasks.Positioned.Object (The_Task).Pose);
            end if;
         end;
      elsif The_Task in Tasks.Wander.Object'Class then
         Do_Wander (This, Tasks.Wander.Object (The_Task));
      end if;

      --  Additional processing for Gaps At Pose when arrived at pose.
      --           if The_Task in Tasks.Gaps_At_Pose.Object'Class and then Done then
      --              declare
      --                 T : Tasks.Gaps_At_Pose.Object renames
      --                   Tasks.Gaps_At_Pose.Object (The_Task);
      --                 Gaps : Sancta.Gap.Object_Array :=
      --                          Extract_Gaps
      --                            (Get_Laser_Scan (This),
      --                             Horizon => 8.0);
      --                 PData : Tasks.Types.Pursuit_Info renames
      --                   Ref (T.Pursuit_Data).all;
      --              begin
      --                 To_World (Gaps, Get_Pose (This));
      --                 T.Gaps := Sancta.Gap.Vector.To_Vector
      --                   (Sancta.Gap.Vector.Item_Array (Gaps));
      --
      --                 --  Update gaps for GUI.
      --                 PData.Gaps.Include (This.Get_Name, T.Gaps);
      --              end;
      --           end if;

      --  Holding Pose
      --           if The_Task in Tasks.Hold_Pose.Object'Class and then Done then
      --              Do_Hold_Pose (This, Tasks.Hold_Pose.Object (The_Task), Done);
      --           end if;

      --  Track two points.
      --           if The_Task in Tasks.Track_Two_Points.Object then
      --              Do_Track_Two_Points (This,
      --                                   Tasks.Track_Two_Points.Object (The_Task),
      --                                   Done);
      --           end if;

      --  Still
      if The_Task in Sancta.Tasks.Still.Object'Class then
         if Sancta.Tasks.Still.Object (The_Task).Is_Still then
            Set_Vel (This, 0.0, 0.0, 0.0);
         else
            Done := True;
         end if;
      end if;

      --  Common info for pursuit tasks
      --           if The_Task in Tasks.Gaps_At_Pose.Object then
      --              Do_Pursuit_Common
      --                (This,
      --                 Ref (Tasks.Gaps_At_Pose.Object (The_Task).Pursuit_Data).all);
      --           elsif The_Task in Tasks.Track_Two_Points.Object then
      --              Do_Pursuit_Common
      --                (This,
      --                 Tasks.Types.Smart_Pursuit_Info.Ref
      --                   (Tasks.Track_Two_Points.Object (The_Task).Get_Pursuit_Data).all);
      --           end if;

      --  Registered tasks (new mechanism, should obsolesce others:
      if This.Has_Executer (The_Task) then
         This.Call_Executer (The_Task, Done);
      end if;

      if The_Task /= Original then
         --  Propagate task changes:
         Update_Gui_Current_Task (This, The_Task);
      end if;

   end Execute;

   -----------------------
   -- Execute_When_Idle --
   -----------------------

   procedure Execute_When_Idle
     (This : in out Object;
      Plan : in out Sancta.Plan.Object)
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      pragma Unreferenced (Plan);
   begin
      if not This.Is_Connected then
         return;
         --  raise Connection_With_Robot_Lost;
      end if;

      if This.Executing then
         Trace.Log ("Robot stopped.", Trace.Debug);
         Set_Vel (This, 0.0, 0.0, 0.0);
         This.Executing := False;
         Update_Gui_Command (This, "Stop");
      end if;

      Update_Gui_Pose (This);
      Check_Gui_Tasks (This);
   end Execute_When_Idle;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (This : in Object; From, To : in Sancta.Tasks.Object'Class)
                      return Sancta.Costs
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      return Sancta.Cost_Utils.Get_Execution_Cost
        (From, To,
         Get_Pose (This),
         Get_Top_Linear_Speed  (This),
         Get_Top_Angular_Speed (This));
   end Get_Cost;

   -------------------
   -- Get_Last_Pose --
   -------------------

   function Get_Last_Pose (This : in Object) return Types.Pose is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      use Sancta.Tasks.Containers.Lists;
      L : constant List := Get_Tasks (This);
      I : cursor        := Last (L);
   begin
      while I /= No_Element loop
         if Element (I) in Tasks.Positioned.Object'Class then
            return Tasks.Positioned.Object (Element (I)).Pose;
         else
            Previous (I);
         end if;
      end loop;

      return Get_Pose (This);
   end Get_Last_Pose;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : in Object) return Types.Pose is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      pragma Unreferenced (This);
   begin
      return Datastore.Get_Pose (Key_Current_Pose);
   exception
      when Agpl.Protected_Datastore.Data_Not_Present =>
         return Types.Null_Pose;
   end Get_Pose;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose (This : in out Object; Pose : in Types.Pose) is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Datastore.Object.Set (String (Component.Data_Manual_Pose),
                            Datastore.Pose'(Pose => Pose));
   end Set_Pose;

   ---------------
   -- Goto_Pose --
   ---------------

   procedure Goto_Pose (This : in out Object; Pose : in Types.Pose) is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      Odom_Pose : constant Types.Pose := Datastore.Get_Pose (Key_Odom_Pose);
   begin
      Datastore.Object.Put (Key_Goal_Action,
                            Component.Goal_Action'
                              (Kind => Component.Set_Goal,
                               Goal => Types.Transformations.World_To_Odom
                                 (Odometry_Pose => Odom_Pose,
                                  World_Pose    => This.Get_Pose,
                                  Goal_Pose     => Pose)));
   end Goto_Pose;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (This : in Object) return Types.Pose is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      return (1.0, 0.7, 0.0);
   end Get_Size;

   -------------
   -- Get_Vel --
   -------------

   function Get_Vel (This : in Object) return Types.Pose is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      return Datastore.Get_Pose (Key_Current_Velocity);
   exception
      when Agpl.Protected_Datastore.Data_Not_Present =>
         return Types.Null_Pose;
   end Get_Vel;

   -------------
   -- Set_Vel --
   -------------

   procedure Set_Vel (This    : in out Object;
                      X, Y, A : in     Types.Real)
   is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Datastore.Object.Put (Key_Goal_Action,
                            Component.Goal_Action'
                              (Kind => Component.Set_Vel,
                               Vel  => (X, Y, Types.Angle (A))));
   end Set_Vel;

   ---------------------------
   -- Get_Top_Angular_Speed --
   ---------------------------

   function Get_Top_Angular_Speed (This : in Object) return Types.Real is
      pragma Unreferenced (This);
   begin
      return 0.5;
   end Get_Top_Angular_Speed;

   --------------------------
   -- Get_Top_Linear_Speed --
   --------------------------

   function Get_Top_Linear_Speed (This : in Object) return Types.Real is
      pragma Unreferenced (This);
   begin
      return 0.5;
   end Get_Top_Linear_Speed;

   --------------------------
   -- Get_Closest_Obstacle --
   --------------------------

   function Get_Closest_Obstacle
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Reading
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      Scan : constant Types.Range_Scan := Get_Laser_Scan (This, From, To);
      Ang  : Types.Angle := 0.0;
      Dist : Types.Real  := Types.Real'Last;
      use type Types.Real;
   begin
      for I in Scan'First .. Scan'Last loop
         if Scan (I).D <= +Dist then
            Ang  := +Scan (I).A;
            Dist := +Scan (I).D;
         end if;
      end loop;

      return (A => Ang, D => +Dist);
   end Get_Closest_Obstacle;

   --------------------
   -- Get_Laser_Scan --
   --------------------

   function Get_Laser_Scan
     (This : Object;
      From : Types.Angle := -Pi;
      To   : Types.Angle :=  Pi) return Types.Range_Scan
   is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      if Datastore.Object.Contains (Key_Laser_Scan) then
         declare
            Result : Types.Range_Scan (1 .. 1000);
            Idx    : Positive := Result'First;
            Scan   : constant Datastore.Range_Scan_Access :=
                       Datastore.Posed_Range_Scan
                         (Datastore.Object.Get (Key_Laser_Scan)).Scan.Ref;
         begin
            for I in Scan'Range loop
               if Scan (I).A >= From and then
                 Scan (I).A <= To
               then
                  Result (Idx) := Scan (I);
                  Idx          := Idx + 1;
               end if;
            end loop;

            return Result (Result'First .. Idx - 1);
         end;
      else
         return Types.Range_Scan'(1 .. 0 => <>);
      end if;
   end Get_Laser_Scan;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (This : in Object) return Boolean is
      pragma Unreferenced (This);
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      return Datastore.Get_Boolean (Key_Is_Connected);
   end Is_Connected;

   ------------------------
   -- Register_Executers --
   ------------------------

   procedure Register_Executers (This : in out Object) is
   begin
      This.Add_Task_Executer (Tasks.Explore_Directed_Segment.Object'Tag,
                              Do_Explore_Directed_Segment'Access);
      This.Add_Task_Executer (Tasks.Goto_Pose.Object'Tag,
                              Do_Goto_Pose'Access);
      This.Add_Task_Executer (Tasks.Speed_Driving.Object'Tag,
                              Do_Speed_Driving'Access);
      This.Add_Task_Executer (Tasks.Wait_For_Orders.Object'Tag,
                              Do_Wait_For_Orders'Access);
   end Register_Executers;

   ---------------------
   -- Check_Gui_Tasks --
   ---------------------

   procedure Check_Gui_Tasks (This : in out Object) is
      use Sancta.Tasks.Containers.Lists;
   begin
      if This.Get_Tasks /= This.Gui_Tasks then
         This.Gui_Tasks := This.Get_Tasks;
         Set_Gui_Tasks (This);
      end if;
   end Check_Gui_Tasks;

   -------------------
   -- Set_Gui_Tasks --
   -------------------

   procedure Set_Gui_Tasks (This : in out Object) is
   begin
      Gui.Robot_Data.Handle.Ref (This.Gui_Data).Set_Tasks (Get_Tasks (This));
      Gui.Robot_Data.Messages.Signal
        (This.Gui_Manager,
         Gui.Robot_Data.Tasks,
         + This.Gui_Data);

      This.Gui_Manager2.Signal (Gui.Robot_Data.All_Tasks,
                                (Network.Message with
                                 Kind   => Gui.Robot_Data.All_Tasks,
                                 Tasks  => This.Get_Tasks,
                                 Cost   => This.Get_Plan_Cost));
   end set_Gui_Tasks;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (This : in out Object; The_Task : in Sancta.Tasks.Object'Class)
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Sancta.Agent.Add_Task (Sancta.Agent.Object (This), The_Task);
   end Add_Task;

   -----------------
   -- Clear_Tasks --
   -----------------

   procedure Clear_Tasks (This : in out Object) is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Sancta.Agent.Clear_Tasks (Sancta.Agent.Object (This));
   end Clear_Tasks;

   function Get_Task_Count (This : in Object) return Natural is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      return Natural (This.Get_Tasks.Length);
   end Get_Task_Count;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : in Object)
                       return Sancta.Tasks.Containers.Lists.List is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
      T : constant Sancta.Tasks.Containers.Lists.List :=
            Sancta.Agent.Object (This).Get_Tasks;
   begin
      return T;
   end Get_Tasks;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This  : in out Object;
                        Tasks : in     Sancta.Tasks.Containers.Lists.List)
   is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Sancta.Agent.Set_Tasks (Sancta.Agent.Object (This), Tasks);
   end Set_Tasks;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task (This : in out Object; Id : in Sancta.Tasks.Task_Id) is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      Sancta.Agent.Remove_Task (Sancta.Agent.Object (This), Id);
   end Remove_Task;

   --------------------
   -- Emergency_Stop --
   --------------------

   procedure Emergency_Stop (This : in out Object) is
      M : Monitor.Object (Sem'Access); pragma Unreferenced (M);
   begin
      if not This.Emergency_Stopped then
         This.Emergency_Stopped := True;
         Datastore.Object.Put (Key_Emergency_Stop,
                               Datastore.Bool'(Value => True));
         delay 1.0; -- Give time for the watchdog component to stop the robot
         Config.Destroy_Plugins;
      end if;
   end Emergency_Stop;

   ------------------------
   -- Update_Gui_Command --
   ------------------------

   procedure Update_Gui_Command (This : in out Object; Command : String) is
      use Gui.Robot_Data.Handle;
   begin
      if Command /= S (This.Gui_Command) then
         This.Gui_Command := U (Command);

         Ref (This.Gui_Data).Set_Attribute
           (Gui.Robot_Data.Last_Command,
            Command);

         Gui.Robot_Data.Messages.Signal
           (This.Gui_Manager,
            Gui.Robot_Data.Command,
            + This.Gui_Data);

         This.Gui_Manager2.Signal (Gui.Robot_Data.Status,
                                (Network.Message with
                                 Kind   => Gui.Robot_Data.Status,
                                 Info   => +Command));
      end if;
   end Update_Gui_Command;

   ---------------------
   -- Update_Gui_Pose --
   ---------------------

   procedure Update_Gui_Pose (This : in out Object) is
      use Gui.Robot_Data.Handle;
      New_Pose : constant Types.Pose := This.Get_Pose;
   begin
      if New_Pose /= This.Gui_Pose then
         This.Gui_Pose := New_Pose;

         Ref (This.Gui_Data).Set_Attribute
           (Gui.Robot_Data.Localized_Pose,
            To_String (New_Pose));

         Gui.Robot_Data.Messages.Signal
           (This.Gui_Manager,
            Gui.Robot_Data.Pose,
            + This.Gui_Data);

         Gui.Robot_Data.Messages2.Signal
           (This.Gui_Manager2,
            Gui.Robot_Data.Pose,
            (Network.Message with
             Kind     => Gui.Robot_Data.Pose,
             Position => New_Pose,
             Velocity => Get_Vel (This)));
      end if;
   end Update_Gui_Pose;

   -----------------------------
   -- Update_Gui_Current_Task --
   -----------------------------

   procedure Update_Gui_Current_Task
     (This : in out Object;
      Job  : in     Sancta.Tasks.Object'Class)
   is
   begin
      Gui.Robot_Data.Messages2.Signal
        (This.Gui_Manager2,
         Gui.Robot_Data.Current_Task,
         (Network.Message with
          Kind     => Gui.Robot_Data.Current_Task,
          Job      => Sancta.Tasks.Handle.Set (Job)));
   end Update_Gui_Current_Task;

end Sancta.Robot;
