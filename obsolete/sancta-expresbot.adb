 

with Sancta.Config;
with Sancta.Gui.Robot_Data;
with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Component;
with Sancta.Component.Mbicp;

with Sancta.Plan;
with Sancta.Tasks.Containers;
with Agpl.Protected_Datastore;
with Agpl.Trace; use Agpl.Trace;

with Gnat.Os_Lib;

package body Sancta.Expresbot is

   use type Sancta.Tasks.Task_Id;
   use type Network.Node_Id;

   -----------------
   -- Cb_Set_Pose --
   -----------------

   procedure Cb_Set_Pose (This : in out Netlistener.Object'Class;
                          M    : in     Network.Message'Class;
                          Meta : in     Network.Message_Metadata)
   is
      use Component;
      Pose : Network.Messages.Set_Pose_Type renames
        Network.Messages.Set_Pose_Type (M);
   begin
      Log ("Received pose correction from " & Network.Image (Meta.Sender),
           Debug, Section => Log_Section);

      --  Reset mbicp so it doesn't correct our manually altered pose.
      if Config.Get_Plugin (Component.Mbicp.Plugin_Name) /= null then
         Component.Mbicp.Object
           (Config.Get_Plugin (Component.Mbicp.Plugin_Name).all).Reset;
      end if;

      Object (This).Bot.Set_Pose (Pose.Pose);
   end Cb_Set_Pose;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Object) is
   begin
      This.Subscribe (Network.Groups.Emergency_Channel);
      This.Subscribe (Network.Groups.Management_Channel);
      This.Register (Network.Messages.Set_Pose_Type'Tag,
                     Cb_Set_Pose'Access);
   end Init;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
   begin
      if M in Network.Messages.Shutdown_Type'Class then
         -- SHUTDOWN --
         This.Bot.Clear_Tasks;
         This.Shutdown := True;
         Log ("Shutdown command received.", Informative);
         --  Unclean exit:
         Gnat.Os_Lib.Os_Exit (0);
      elsif M in Network.Messages.Clear_Tasks_Type'Class or else
            M in Network.Messages.Stop_Type'Class
      then
         -- STOP --
         -- CLEAR TASKS --
         This.Bot.Clear_Tasks;
         Log ("Cleared all tasks.", Informative);
      elsif M in Network.Messages.Set_Tasks_Type'Class then
         -- SET_TASKS --
         This.Bot.Set_Tasks (Network.Messages.Set_Tasks_Type (M).Jobs);
         Log ("Received" & This.Bot.Get_Tasks.Length'Img & " new tasks.",
              Debug,
              Section => Log_Section);
      end if;
   end Process_Incoming_Packet;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean)
   is
      Plan : Sancta.Plan.Object;
   begin
      Done := This.Shutdown;

      --  Message dispatching
      This.Run; -- inherited from netlistener

      --  Execute the agent things:
      if This.Bot.Has_Tasks then
         declare
            use Sancta.Tasks.Containers.Lists;
            Tasks    : List := This.Bot.Get_Tasks;
            First    : Sancta.Tasks.Primitive.Object'Class :=
                         Sancta.Tasks.Primitive.Object(Element (Tasks.First));
            Job_Done : Boolean := False;
         begin
            This.Bot.Execute (First, Plan, Job_Done);
            Tasks.Delete_First;
            if not Job_Done then
               Tasks.Prepend (First); -- with changes
               This.Bot.Set_Tasks (Tasks);
            else
               --  Notify:
               Task_Finished (Object'Class (This), First);

               --  Network notification
               This.Link.Multicast (Network.Groups.Emergency_Channel,
                                    Network.Messages.Task_Done (First.Get_Id));

               Log ("Task " & First.To_String & " finished.",
                    Trace.Debug, Section => Log_Section);

               This.Bot.Set_Tasks (Tasks);
            end if;
         end;
      else
         This.Bot.Execute_When_Idle (Plan);
      end if;

      --  Send a position update every split second:
      if This.Pose_Cron.Elapsed >= 0.5 then
         This.Pose_Cron.Reset;

         This.Link.Multicast (Network.Groups.Gui_Channel,
                              Gui.Robot_Data.Network_Update'
                                (Network.Message with
                                 Kind     => Gui.Robot_Data.Pose,
                                 Sequence => <>,
                                 Position => This.Bot.Get_Pose,
                                 Velocity => (0.0, 0.0, 0.0)));
         pragma Incomplete ("Velocity should be passed");

         begin
            This.Link.Multicast (Network.Groups.Gui_Channel,
                                 Network.Messages.Laser);
         exception
            when Agpl.Protected_Datastore.Data_Not_Present =>
               null;
         end;
      end if;
   end Run;

end Sancta.Expresbot;
