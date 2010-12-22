with Sancta.Datastore;
with Sancta.Network.Consumer.Root;
with Sancta.Network.Groups;
with Sancta.Network.Messages;
with Sancta.Component.Factory;
pragma Elaborate_All (Sancta.Component.Factory);

with Agpl.Trace; use Agpl.Trace;

--  with Ada.Text_Io; use Ada.Text_Io;

package body Sancta.Component.Task_Manager is

   use Ada.Calendar;
   package Task_Lists renames Sancta.Tasks.Containers.Lists;

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Link :=
        Network.Layer.Object_Access
          (Datastore.Network_Layer
               (Datastore.Object.Get (Link_Key)).Ref);

      Network.Consumer.Root.Subscribe
        (This.Inbox'Access, This.Link, Network.Groups.Emergency_Channel);

      --  Store initial empty list
      Datastore.Object.Put (This.Key (Provides_Task_List),
                            Shared_Task_List'(List => Task_Lists.Empty_List));

      return Component.Object_Access (This);
   end Create;

   ----------------------
   -- Process_Messages --
   ----------------------

   procedure Process_Messages (This : in out Object) is
   begin
      while not This.Inbox.Is_Empty loop
         declare
            M : constant Network.Message'Class := This.Inbox.Get_First;
         begin
            This.Inbox.Remove_First;

            if M in Network.Messages.Set_Tasks_Type then
               -- SET_TASKS --
               declare
                  Msg : Network.Messages.Set_Tasks_Type renames
                    Network.Messages.Set_Tasks_Type (M);
               begin
                  Datastore.Object.Put
                    (This.Key (Provides_Task_List),
                     Shared_Task_List'(List => Msg.Jobs));
                  Log ("Set" & Msg.Jobs.Length'Img & " tasks.",
                       Debug, Section => Log_Section);
               end;
            else
               null;
            end if;
         end;
      end loop;
   end Process_Messages;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time) is
   begin
      Process_Messages (This);

      Next := Clock + 0.01;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Task_Manager;
