with Sancta.Ctree.Robot;

with Sancta.Tasks.Positioned;

with Sancta.Agent.Containers;

package body Sancta.Ctree.Data_Server.Utils is

   package Agent_Lists renames Sancta.Agent.Containers.Lists;

   ---------------
   -- Get_Poses --
   ---------------

   procedure Get_Poses (Bots :        Natural;
                        Ass  : in out Sancta.Assignment.Object)
   is
   begin
      for I in 1 .. Bots loop
         declare
            Bot_Id  : constant Robot_Id := Value (I'Img);
            New_Bot : Robot.Object;
            Pose    : Poses;
         begin
            if Ass.Contains (Image (Bot_Id)) then
               New_Bot := Robot.Object
                 (Ass.Get_Agent (Image (Bot_Id)));
            else
               New_Bot.Set_Name (Sancta.Ctree.Image (Sancta.Ctree.Value (I'Img)));
            end if;
            Get_Pose (Bot_Id, Pose);
            New_Bot.Set_Pose (+Pose);
            Ass.Set_Agent (New_Bot);
         end;
      end loop;
   end Get_Poses;

   ---------------
   -- Set_Goals --
   ---------------

   procedure Set_Goals (Bots : Natural;
                        Ass  : Sancta.Assignment.Object)
   is
      Ag : constant Agent_Lists.List   := Ass.Get_Agents;
      I  :          Agent_Lists.Cursor := Ag.First;
   begin
      --  Set default "no goal" for all
      for I in 1 .. Bots loop
         Set_Goal (Value (I'Img), Goal (0.0, 0.0, Active => False));
      end loop;

      --  Set assigned goals
      while Agent_Lists.Has_Element (I) loop
         declare
            A : constant Robot.Object'Class :=
                  Robot.Object (Agent_Lists.Element (I));
         begin
            if A.Has_Tasks then
               declare
                  T : constant Sancta.Tasks.Positioned.Object'Class :=
                        Sancta.Tasks.Positioned.Object'Class (A.Get_First_Task);
               begin
                  Set_Goal (Value (A.Get_Name),
                            Goal (Float (T.Pose.X),
                                  Float (T.Pose.Y),
                                  Active => True));
               end;
            end if;
         end;
         Agent_Lists.Next (I);
      end loop;
   end Set_Goals;

end Sancta.Ctree.Data_Server.Utils;
