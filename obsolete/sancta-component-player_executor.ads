with Sancta.Component.Root;

with Player.Position2d;

package Sancta.Component.Player_Executor is

   Plugin_Name : constant String := "player_executor";

   Log_Section : constant String := "sancta.component.player_executor";

   --  Requires_Robot_Action : constant Data_Key := "robot_action";
   --  Hardwired as Action_Key
   --  Requires_World_Pose : constant Data_Key := "world_pose";
   --  Hardwired as World_Pose_Key

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      Pos : Player.Position2d.Object; -- Will subscribe to position2d:0
   end record;

end Sancta.Component.Player_Executor;
