with Agpl.Strings,
     Sancta.Agent,
     Sancta.Assignment,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Containers,
     Sancta.Tasks,
     Sancta.Tasks.Positioned,
     Sancta.Tasks.Speed_Driving;

use Sancta.Containers;

package body Sancta.Component.Team_Splitter is

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : Agpl.Xml.Node)
      return Component.Object_Access
   is
      use Agpl.Strings,
          Utils;
      This : constant Object_Access := new Object (Name'Access, Config);
      Num  : constant Natural       := Option (This.all, Option_Amount, 0);

   begin
      for I in 1 .. Num loop
         This.Add_Correspondence
           (Internal_Key (String (Provides_Pose_Goal)  & (Trim (I'Img))),
            This.Ekey    (Provides_Pose_Goal) & External_Key (Trim (I'Img)));
         This.Add_Correspondence
           (Internal_Key (String (Provides_Velo_Goal)  & (Trim (I'Img))),
            This.Ekey    (Provides_Velo_Goal) & External_Key (Trim (I'Img)));
      end loop;

      This.Subscribe (Requires_Team);

      return Component.Object_Access (This);
   end Create;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored
     (This  : in out Object;
      Key   : in     Internal_Key;
      Value : in     Data'Class)
   is
      pragma Unreferenced (Key);
      Team : Assignment.Object renames Types.Teams (Value).Team;
      Idx  : Positive := 1;

      procedure Command (I : Ac.Lists.Cursor) is
         Bot : constant Agent.Object'Class :=
                 Agent.Object'Class (Ac.Lists.Element (I));
      begin
         Log ("Sending split orders for " & Bot.Get_Name, Debug, Log_Section);
         if not Bot.Has_Tasks then
            This.Output (Provides_Velo_Goal, Idx,
                         Types.Pose'(Data with (0.0, 0.0, 0.0)));
         else
            declare
               Job : constant Tasks.Object'Class := Bot.Get_First_Task;
            begin
               if Job in Tasks.Speed_Driving.Object'Class then
                  This.Output
                    (Provides_Velo_Goal, Idx,
                     Types.Pose'
                       (Data with Tasks.Speed_Driving.Object (Job).Velocity));
               elsif Job in Tasks.Positioned.Object'Class then
                  This.Output
                    (Provides_Pose_Goal, Idx,
                     Types.Pose'
                       (Data with Tasks.Positioned.Object (Job).Pose));
               else
                  raise Constraint_Error
                    with "Task can't be interpreted: " & Job.Image;
               end if;
            end;
         end if;
         Idx := Idx + 1;
      end Command;

   begin
      Log ("Splitting team...", Debug, Log_Section);
      Team.Get_Agents.Iterate (Command'Access);
   end Key_Stored;

end Sancta.Component.Team_Splitter;
