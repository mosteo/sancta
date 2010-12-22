--  Sample empty component to save-as when creating new ones.

with Sancta.Network.Inbox;
with Sancta.Network.Layer;
with Sancta.Component.Root;

with Sancta.Tasks.Containers;
with Agpl.Protected_Datastore;

package Sancta.Component.Task_Manager is

   Plugin_Name : constant String := "task_manager";

   Log_Section : constant String := "sancta.component.task_manager";

   --  Requires_Link      : constant Data_Key := "link";
   --  Hardwired as Link_Key

   Provides_Task_List : constant Internal_Key := "task_list";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   --  Any of the following subprograms can be removed entirely if they're not
   --  going to be used, since there's null defaults in the Root class.

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   type Shared_Task_List is new Agpl.Protected_Datastore.Object_Data with record
      List : Sancta.Tasks.Containers.Lists.List;
   end record;

   --  Functions to manipulate a given task list
   --  To be added when needed...

private

   type Object is new Root.Object with record
      Link     : Network.Layer.Object_Access;

      Inbox    : aliased Network.Inbox.Object;
   end record;

end Sancta.Component.Task_Manager;
