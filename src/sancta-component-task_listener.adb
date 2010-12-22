with
     Sancta.Component.Factory,
     Sancta.Component.Helper,
     Sancta.Component.Network,
     Sancta.Component.Types,
     Sancta.Network.Messages,
     Sancta.Starter;


package body Sancta.Component.Task_Listener is

   type Object_Access is access all Object;

   package Net renames Sancta.Network;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register
        (Name, Create'Access,
         (Requires_Agent'Access,
          Requires_Link'Access));
   end Register;

   -----------------------------
   -- Process_Incoming_Packet --
   -----------------------------

   procedure Process_Incoming_Packet
     (This : in out Listener_Type;
      M    : in     Sancta.Network.Message'Class;
      Meta : in     Sancta.Network.Message_Metadata)
   is
      pragma Unreferenced (Meta);
      package Nm renames Sancta.Network.Messages;
   begin
      Log ("Received msg: " & External_Tag (M'Tag), Debug, Log_Section);
      if M in Nm.Shutdown_Type then
         Starter.Shutdown;
--        elsif M in Nm.Set_Pose_Type
--          and then This.Parent.Agent.all in Located_Agent.Object'Class
--        then
--           Located_Agent.Object'Class (This.Parent.Agent.all).Set_Pose
--             (Nm.Set_Pose_Type (M).Pose);
      elsif M in Nm.Set_Tasks_Type then
         This.Parent.Agent.Set_Tasks (Nm.Set_Tasks_Type (M).Jobs);
--           Log ("First task of received: " &
--                This.Parent.Agent.Get_First_Task.To_String, Always);
      else
         Log ("Dropping message: " & External_Tag (M'Tag),
              Warning, Log_Section);
      end if;
   end Process_Incoming_Packet;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node;
      Env    : Environment.Object)
      return Component.Object_Access
   is
      pragma Unreferenced (Env);
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object (Name'Access,
                           Config,
                           Network.Network (Help.Input (Requires_Link)).Link,
                           Types.Agent (Help.Input (Requires_Agent)).Agent);
   begin
      This.Verify (Option_Channel);
      This.Listener.Subscribe (Net.Value (This.Option (Option_Channel, "")));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      This.Listener.Run;
      Next := Clock + 0.01;
   end Run;

end Sancta.Component.Task_Listener;
