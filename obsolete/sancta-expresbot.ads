 

with Sancta.Netlistener;
with Sancta.Network.Layer;
with Sancta.Robot_Hard;

with Agpl.Chronos;
with Sancta.Tasks.Primitive;
with Agpl; use Agpl;

--  This ExpresBot reunites functionality of netbot, simplebot and simplebot2.
--  The idea is to stop extending these anymore, and make everything a component.
--  So the expresbot at now manages basic functionality that may be removed in
--  the future.

--  Messages currently accepted:
--  STOP
--  ADD_TASK
--  INCLUDE_TASK
--  SET_TASKS
--  CLEAR_TASKS
--  SHUTDOWN


package Sancta.Expresbot is

   --  pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.expresbot";
   Detail_Section : constant String := "sancta.expresbot.detail";
   --  To selectively enable debug messages...

   type Object (Bot   : not null access Robot_Hard.Object'Class;
                Link  : not null access Network.Layer.Object'Class)
     is new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   not overriding
   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Done will be true when the bot has finished operation.
   --  Once this happens Run shouldn't be called again.

   not overriding
   procedure Task_Finished (This : in out Object;
                            Job  : in out Sancta.Tasks.Primitive.Object'Class)
   is null;
   --  Will be called when a task is finished.
   --  Override if needed.

private

   type Object (Bot   : not null access Robot_Hard.Object'Class;
                Link  : not null access Network.Layer.Object'Class)
     is new Netlistener.Object (Link => Link) with
      record
         Pose_Cron : Agpl.Chronos.Object;
         Shutdown  : Boolean := False;
      end record;

   not overriding
   procedure Cb_Set_Pose (This : in out Netlistener.Object'Class;
                          M    : in     Network.Message'Class;
                          Meta : in     Network.Message_Metadata);

   overriding
   procedure Init (This : in out Object);

   overriding
   procedure Process_Incoming_Packet (This : in out Object;
                                      M    : in     Network.Message'Class;
                                      Meta : in     Network.Message_Metadata);

end Sancta.Expresbot;
