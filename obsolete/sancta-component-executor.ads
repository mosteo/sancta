private with Sancta.Network.Inbox;
private with Sancta.Network.Layer;
with Sancta.Component.Root;

private with Sancta.Tasks;

package Sancta.Component.Executor is

   Plugin_Name : constant String := "executor";

   --  Requires_Link      : constant Data_Key := "link";
   --  Hardwired as Link_Key

   Requires_World_Pose : constant Internal_Key := "world_pose";
   Requires_Task_List  : constant Internal_Key := "task_list";

   Provides_Action     : constant Internal_Key := "action";

   type Object is new Root.Object with private;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   type Object is new Root.Object with record
      Link     :         Network.Layer.Object_Access;
      Inbox    : aliased Network.Inbox.Object;
   end record;

   not overriding
   procedure Do_Goto_Pose (This : in out Object;
                           Job  : in out Sancta.Tasks.Object'Class;
                           Done : in out Boolean);

   not overriding
   procedure Execute (This : in out Object);

end Sancta.Component.Executor;
