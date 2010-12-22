with Sancta.Agent_Proxy;
--  with Sancta.Debug2;
--  with Sancta.Network.Groups;
with Sancta.Network.Messages;

with Sancta.Tasks.Handle;
with Agpl.Trace; use Agpl.Trace;

with Ada.Calendar; use Ada.Calendar;

package body Sancta.Planner_Central is

   use type Gui.Robot_Data.Network_Update_Kinds;


   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method (This   : in out Object;
                         Method : in     Sancta.Method.Object'Class)
   is
   begin
      Sancta.Plan.Add_Method (This.Plan, Method);
   end Add_Method;

   -------------------------
   -- Add_Or_Update_Agent --
   -------------------------

   procedure Add_Or_Update_Agent (This : in out Object;
                                  Meta : in     Network.Message_Metadata)
   is
      use Sancta.Agent.Containers.Maps;
      Name : constant String := Image (Network.Sender (Meta));
      I    : constant Cursor := Find (This.Agents, Name);

      procedure Set_Alive (Key : in String; X : in out Sancta.Agent.Object'Class) is
         pragma Unreferenced (Key);
         Agent : Agent_Proxy.Object renames Agent_Proxy.Object (X);
      begin
         if Agent.Get_Alive then
            Log ("Agent " & Name & " is alive", Never, Section => Detail_Section);
         else
            Log ("Agent " & Name & " has reappeared", Debug, Section => Log_Section);
         end if;
         Agent_Proxy.Set_Alive (Agent_Proxy.Object (X));
      end Set_Alive;
   begin
      if Has_Element (I) then
         Update_Element (This.Agents, I, Set_Alive'Access);
      else
         declare
            New_Agent : Sancta.Agent_Proxy.Object;
         begin
            New_Agent.Set_Id (Network.Sender (Meta));
            New_Agent.Set_Name (Name);
            New_Agent.Set_Alive;
            This.Agents.Include (Name, New_Agent);
            Log ("Agent " & Name & " has appeared", Debug, Section => Log_Section);
         end;
      end if;
   end Add_Or_Update_Agent;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task (This : in out Object;
                       Job  : in     Sancta.Tasks.Object'Class)
   is
   begin
      This.Pending_Tasks.Append (Job);
      This.Last_Task_Time.Reset;
      This.Replan_Needed := True;
      Log ("Received task: " & Job.To_String, Debug,
           Section => Detail_Section);
   end Add_Task;

   ---------------------
   -- Check_Lost_Bots --
   ---------------------

   procedure Check_Lost_Bots (This : in out Object) is
      use Sancta.Agent.Containers.Maps;

      procedure Check_Bot (Key : in String; X : in out Sancta.Agent.Object'Class) is
         Bot : Agent_Proxy.Object renames Agent_Proxy.Object (X);
      begin
         if Bot.Get_Alive and then Clock - Bot.Get_Last_Seen > This.Aliveness then
            Bot.Set_Alive (False);
            Log ("Agent " & Key & " has been lost, marked DEAD", Warning);
         end if;
      end Check_Bot;

      procedure Check_All (X : in Cursor) is
      begin
         Update_Element (This.Agents, X, Check_Bot'Access);
      end Check_All;

   begin
      This.Agents.Iterate (Check_All'Access);
   end Check_Lost_Bots;

   ----------------------
   -- Get_Alive_Agents --
   ----------------------

   function Get_Alive_Agents (This : in Object) return Sancta.Agent.Containers.Lists.List is
      use Sancta.Agent.Containers.Lists;
      Agents : List;

      procedure Add_If_Alive (X : in Sancta.Agent.Containers.Maps.Cursor) is
         use Sancta.Agent.Containers.Maps;
      begin
         if Agent_Proxy.Object (Element (X)).Get_Alive then
            Append (Agents, Element (X));
         end if;
      end Add_If_Alive;

   begin
      This.Agents.Iterate (Add_If_Alive'Access);
      return Agents;
   end Get_Alive_Agents;

   -----------------------
   -- Get_Pending_Tasks --
   -----------------------

   function Get_Pending_Tasks (This : in Object) return Sancta.Tasks.Containers.Lists.List is
   begin
      return This.Pending_Tasks;
   end Get_Pending_Tasks;

   -----------------
   -- Get_Planner --
   -----------------

   function Get_Planner (This : in Object) return Sancta.Plan.Object is
   begin
      return This.Plan;
   end Get_Planner;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      Subscribe (This, Network.Groups.Channel_Tags (Network.Groups.Gui_Channel));
      Subscribe (This, Network.Groups.Channel_Tags (Network.Groups.Emergency_Channel));
      Subscribe (This, Network.Groups.Channel_Tags (Network.Groups.Management_Channel));
   end Init;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
   begin
      if M in Network.Messages.Propose_Task_Type'Class then
         declare
            X : Network.Messages.Propose_Task_Type renames
              Network.Messages.Propose_Task_Type (M);
            use Sancta.Tasks.Handle;
         begin
            Add_Task (This, Get (X.Job));
         end;
      elsif M in Network.Messages.Hello_Type'Class then
         Add_Or_Update_Agent (This, Meta);
      elsif (M in Gui.Robot_Data.Network_Update'Class) and then
            Gui.Robot_Data.Network_Update (M).Kind = Gui.Robot_Data.Pose
      then
         Add_Or_Update_Agent (This, Meta);
         Update_Agent_Pose (This,
                            Gui.Robot_Data.Network_Update (M),
                            Meta);
      end if;
   end Process_Incoming_Packet;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean)
   is
   begin
      Netlistener.Run (Netlistener.Object (This));

      --  Check for lost bots
      This.Check_Lost_Bots;

      --  Do assignation
      if This.Replan_Needed and then
        This.Last_Task_Time.Elapsed >= This.Wait and then
        not This.Pending_Tasks.Is_Empty
      then
         This.Replan_Needed := False;
         Log ("Planning for" & This.Pending_Tasks.Length'Img & " pending tasks",
              Informative, Section => Log_Section);
         Replan (Object'Class (This));
         This.Pending_Tasks.Clear;
    end if;
   end Run;

   -------------------------
   -- Send_Plan_To_Agents --
   -------------------------

   procedure Send_Plan_To_Agents (This       : in Object;
                                  Assignment : in Sancta.Assignment.Object)
   is
      use Sancta.Agent.Containers.Lists;

      Agents : constant Sancta.Agent.Containers.Lists.List := Sancta.Assignment.Get_Agents (Assignment);
      I      :          Cursor := Agents.First;
   begin
      while Has_Element (I) loop
         This.Link.Send
           (Agent_Proxy.Object (Element (I)).Get_Id,
            Network.Messages.Set_Tasks (Sancta.Assignment.Get_Tasks (Assignment,
                                                                 Element (I))));
         Log ("New tasks sent to " & Element (I).Get_Name,
              Debug, Section => Detail_Section);
         Next (I);
      end loop;
   end Send_Plan_To_Agents;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration (This      : in out Object;
                                Wait      : in     Duration := Default_Wait;
                                Aliveness : in     Duration := Default_Aliveness)
   is
   begin
      This.Aliveness := Aliveness;
      This.Wait      := Wait;
   end Set_Configuration;

   -----------------------
   -- Update_Agent_Pose --
   -----------------------

   procedure Update_Agent_Pose (This : in out Object;
                                M    : in     Gui.Robot_Data.Network_Update'Class;
                                Meta : in     Network.Message_Metadata)
   is
      use Sancta.Agent.Containers.Maps;
      Name : constant String    := Network.Image (Network.Sender (Meta));

      procedure Update_Pose (Key : in String; Bot : in out Sancta.Agent.Object'Class)
      is
         pragma Unreferenced (Key);
      begin
--           Log ("Updating " & Bot.Get_Name &
--                " pose to " & Debug2.To_String (M.Position), Debug,
--                Section => Detail_Section);
         Agent_Proxy.Object (Bot).Set_Pose (M.Position);
      end Update_Pose;

   begin
      pragma Assert (M.Kind = Gui.Robot_Data.Pose);

      Update_Element (This.Agents,
                      Find (This.Agents, Name),
                      Update_Pose'Access);
   end Update_Agent_Pose;

end Sancta.Planner_Central;
