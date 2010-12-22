with Sancta.Agent,
     Sancta.Component.Ctypes,
     Sancta.Component.Factory,
     Sancta.Containers,
     Sancta.Starter,
     Sancta.Tasks.Interfaces,
     Sancta.Tasks.Utils;

use Sancta.Containers;

package body Sancta.Component.Task_Completion_Checker is

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
     (Config : Comp_Config)
      return Component.Object_Access
   is
      This : constant Root.Object_Access := new Object (Name'Access, Config);
   begin
      This.Subscribe (Requires_Agent);
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
      use Tasks.Utils;
   begin
      if not This.Exists (Requires_In_Tasks) then
         Log ("Abandoning check, no tasks yet", Debug, Log_Section);
         return;
      end if;

      if Key = Requires_Agent then
         declare
            Bot : constant Agent.Object'Class :=
                    Agent.Object'Class (Ctypes.Agent (Value).Agent.all);
         begin
            if Bot.Has_Tasks then
               This.Timer_Shutdown.Reset;
            end if;

            if Bot.Has_Tasks and then
              Bot.Get_First_Task in Tasks.Interfaces.Completable'Class and then
              Tasks.Interfaces.Completable'Class (Bot.Get_First_Task).Completed
              (Bot)
            then
               declare
                  In_Tasks : Tc.Lists.List renames
                    Ctypes.Task_List (This.Input (Requires_In_Tasks)).Tasks;
                  Ok_Task  : Tasks.Object'Class renames Bot.Get_First_Task;
               begin
                  Log ("Agent " & Bot.Get_Name & " has completed " &
                       Ok_Task.To_String, Debug, Log_Section);
                  This.Output
                    (Provides_Out_Tasks,
                     Ctypes.Task_List'
                       (Tasks => In_Tasks - Ok_Task));
               end;
            end if;
         end;
      elsif Key = Requires_Team then
         declare
            Pending        : Tc.Lists.List :=
                        Ctypes.Task_List (This.Input (Requires_In_Tasks)).Tasks;
            Some_Completed : Boolean := False;
            procedure Check_Bot (I : Ac.Lists.Cursor) is
               Bot : constant Agent.Object'Class := Ac.Lists.Element (I);
            begin
               if Bot.Has_Tasks and then
                 Bot.Get_First_Task in Tasks.Interfaces.Completable'Class
                 and then
                   Tasks.Interfaces.Completable'Class
                     (Bot.Get_First_Task).Completed (Bot)
               then
                  Pending        := Pending - Bot.Get_First_Task;
                  Some_Completed := True;
                  Log ("Team agent " & Bot.Get_Name & " has completed " &
                       Bot.Get_First_Task.To_String, Debug, Log_Section);
               end if;
            end Check_Bot;
         begin
            if not Pending.Is_Empty then
               This.Timer_Shutdown.Reset;
            end if;

            Ctypes.Teams (Value).Team.Get_Agents.Iterate (Check_Bot'Access);
            if Some_Completed then
               This.Output (Provides_Out_Tasks,
                            Ctypes.Task_List'(Tasks => Pending));
            end if;
         end;
      end if;

      if This.Exists (Option_Shutdown) and then
        Boolean'Value (This.Option (Option_Shutdown)) and then
        Ctypes.Task_List (This.Input (Requires_In_Tasks)).Tasks.Is_Empty and then
        This.Timer_Shutdown.Elapsed >=
          Duration'Value
            (This.Option (Option_Shutdown_Delay, Default_Shutdown_Delay'Img))
      then
         Log ("No tasks pending, shutting down...", Always, Log_Section);
         Sancta.Starter.Shutdown;
      end if;
   end Key_Stored;

end Sancta.Component.Task_Completion_Checker;
