with Sancta.Auction_Messages;
with Sancta.Datastore;
with Sancta.Network.Groups;
with Sancta.Component.Factory;
pragma Elaborate_All (Sancta.Component.Factory);

with Sancta;
with Sancta.Agent.Handle;
with Sancta.Tasks.Insertions;
with Agpl.Trace; use Agpl.Trace;
with Agpl; use Agpl;

package body Sancta.Component.Bidder is

   ------------
   -- Create --
   ------------

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access
   is
      This : constant Object_Access := new Object;
   begin
      This.Prepare (Config);

      This.Bot  := Datastore.Robot
        (Datastore.Object.Get (This.Key (Requires_Agent))).Ref;
      This.Link := Datastore.Network_Layer
        (Datastore.Object.Get (This.Key (Requires_Link))).Ref;

      Network.Layer.Subscribe (This.Link.all,
                               This.Pending_Messages'Access,
                               Network.Groups.Channel_Tags
                                 (Network.Groups.Traderbot_Channel));

      return Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      Next := Clock + 0.05;

      while not This.Pending_Messages.Is_Empty loop
         declare
            M  : constant Network.Message'Class :=
                   This.Pending_Messages.Get_First;
            Mt : constant Network.Message_Metadata :=
                   This.Pending_Messages.Get_First_Metadata;
         begin
            This.Pending_Messages.Remove_First;

            declare
               Result : constant Sancta.Bidder.Outcomes :=
                          This.Bidder.Process_Packet
                            (This.Bot,
                             This.Link,
                             M, Mt);
            begin
               case Result.Kind is
                  when Sancta.Bidder.Award =>
                     declare
                        Cost, Full : Sancta.Costs;
                        New_Agent  : Sancta.Agent.Handle.Object;
                        Success    : Boolean;
                     begin
                        Sancta.Tasks.Insertions.Greedy
                          (This.Bot.all,
                           Auction_Messages.Award_Message (M).Job.Get,
                           New_Agent, Cost, Full, Success);
                        if not Success then
                           Log ("Failed to insert awarded task!",
                                Error, Log_Section);
                        else
                           This.Bot.Set_Tasks (New_Agent.Ref.Get_Tasks);
                        end if;
                     end;
                  when others =>
                     null;
               end case;
            end;
         end;
      end loop;
   end Run;

begin
   Factory.Register (Plugin_Name, Create'Access);
end Sancta.Component.Bidder;
