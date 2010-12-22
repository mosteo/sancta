 

--  Netbot: just implements basic task execution.
--  Notifies when a task has been finished (as expected).
--  Extend it for other purposes.

with Sancta.Netlistener;
with Sancta.Network.Layer;
with Sancta.Robot_Hard;

with Sancta.Tasks.Primitive;
with Agpl; use Agpl;

package Sancta.Netbot is

   --  pragma Elaborate_Body;

   Log_Section    : constant String := "Netbot";
   Detail_Section : constant String := "Netbot.detail";
   --  To selectively enable debug messages...

   type Object (Bot   : not null access Robot_Hard.Object'Class;
                Link  : not null access Network.Layer.Object'Class)
     is abstract new Netlistener.Object with private;

   type Object_Access is access all Object'Class;

   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Extend this with your own operation for the robot.
   --  Done should be true when the bot has finished operation.
   --  Once this happens Run shouldn't be called again.
   --  It's called by Run, you shouldn't call it directly.
   --  Remember that to keep current functionality you should
   --  call this parent implementation first.

   procedure Task_Finished (This : in out Object;
                            Job  : in out Sancta.Tasks.Primitive.Object'Class)
   is null;
   --  Will be called when a task is finished.
   --  Override if needed.

private

   type Object (Bot   : not null access Robot_Hard.Object'Class;
                Link  : not null access Network.Layer.Object'Class)
     is abstract new Netlistener.Object (Link => Link) with null record;

end Sancta.Netbot;
