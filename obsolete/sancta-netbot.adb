with Sancta.Network.Messages;

with Sancta.Plan;
with Sancta.Tasks.Containers;
with Sancta.Tasks.Still;
with Agpl.Trace; use Agpl.Trace;

package body Sancta.Netbot is

   use type Sancta.Tasks.Task_Id;
   use type Node_Id;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Done :    out Boolean) is
      Plan : Sancta.Plan.Object;
   begin
      Done := False; -- Inutil value

      Netlistener.Run (Netlistener.Object (This));

      --  Execute the agent things:
      if This.Bot.Has_Tasks then
         declare
            use Sancta.Tasks.Containers.Lists;
            Tasks : List := This.Bot.Get_Tasks;
            First : Sancta.Tasks.Primitive.Object'Class :=
                      Sancta.Tasks.Primitive.Object (Element (Tasks.First));
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
--                 Log ("Remaining tasks:", Trace.Debug, Section => Log_Section);
--                 Print (Tasks);
               This.Bot.Set_Tasks (Tasks);
            end if;
         end;
      else
         This.Bot.Execute_When_Idle (Plan);
      end if;

      if Done then
         --  Stop the robot
         declare
            Stop : Sancta.Tasks.Still.Object :=
                     Sancta.Tasks.Still.Create (24.0 * 60.0 * 60.0);
            Job_Done : Boolean;
         begin
            This.Bot.Execute (Stop, Plan, Job_Done);
         end;
      end if;
   end Run;

end Sancta.Netbot;
