 

--  Extends Simplebot adding pose beaconing and laser beaconing
--  Accepts pose correction from external source.

with Sancta.Netlistener;
with Sancta.Network;
with Sancta.Simplebot;

with Agpl.Chronos;

package Sancta.Simplebot2 is

--   pragma Elaborate_Body;

   Log_Section    : constant String := "sancta.simplebot2";
   Detail_Section : constant String := "sancta.simplebot2.detail";
   --  To selectively enable debug messages...

   type Object is new Simplebot.Object with private;
   --  The Agent parameter for Netbot must be an instance

   overriding
   procedure Init (This : in out Object);
   --  Register callback for Set_Pose message management

   overriding
   procedure Run (This : in out Object;
                  Done :    out Boolean);
   --  Done should be true when the bot has finished operation.
   --  Once this happens Run shouldn't be called again.

private

   type Object is new Simplebot.Object with
      record
         Pose_Cron : Agpl.Chronos.Object;
      end record;

   procedure Cb_Set_Pose (This : in out Netlistener.Object'Class;
                          M    : in     Network.Message'Class;
                          Meta : in     Network.Message_Metadata);

end Sancta.Simplebot2;
