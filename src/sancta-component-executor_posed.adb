with Agpl.Trace,
     Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Tasks.Goto_Pose,
     Sancta.Tasks.Positioned,
     Sancta.Types.Operations;


package body Sancta.Component.Executor_Posed is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access, (1 => Requires_Agent'Access));
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node;
      Env    :    Environment.Object)
      return Component.Object_Access
   is
      pragma Unreferenced (Env);
      use type Sancta.Agent.Object_Access;
      This : constant Object_Access :=
               new Object (Name'Access, Config);
   begin
      --  Make a copy of this agent, to have it of the same class.
      --  But remove all tasks.
      pragma Assert (This.Get_Agent /= null);
      This.Out_Agent :=
        new Agent.Object'Class'
          (This.Get_Agent.all);
      This.Out_Agent.Clear_Tasks;
      This.Output (Provides_Agent,
                   Types.Agent'(Agent => This.Out_Agent));

      return Component.Object_Access (This);
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This : in out Object;
      Job  : in out Tasks.Object'Class;
      Done :    out Boolean)
   is
      use Sancta.Types.Operations;
      use type Tasks.Object'Class,
          Sancta.Types.Real;
   begin
      This.Stopped := False;
      Done         := False;
      case This.Status is
         when Idle =>
            if Job in Tasks.Positioned.Object'Class then
               Log ("Starting task approach", Informative, Log_Section);
               This.Job.Set (Job);
               This.Status := Going_To_Pose;
               This.Out_Agent.Clear_Tasks;
            end if;
         when Going_To_Pose =>
            if Job /= This.Job.Ref.all then
               Log ("[Going] Task under execution switched!",
                    Warning, Log_Section);
               This.Status := Idle;
               This.Out_Agent.Clear_Tasks;
            else
               if Distance (Types.Pose (This.Input (Requires_Pose)).Pose,
                            Tasks.Positioned.Object (This.Job.Ref.all).Pose) <=
                 This.Dist
               then
                  if This.Job.Ref.all in Tasks.Goto_Pose.Object'Class then
                     This.Status := Idle;
                     Done        := True;
                     Log ("Goto_Pose task completed",
                          Informative, Log_Section);
                  else
                     This.Status := Waiting_Task_Completion;
                     This.Out_Agent.Add_Task (This.Job.Ref.all);
                  end if;
               else
                  This.Output
                    (Provides_Goal,
                     Types.Pose'
                       (Pose => Oriented_Pose
                          (Tasks.Positioned.Object
                             (This.Job.Ref.all).Pose,
                           Types.Pose (This.Input (Requires_Pose)).Pose)));
               end if;
            end if;
         when Waiting_Task_Completion =>
            if Job /= This.Job.Ref.all then
               Log ("[Performing] Task under execution switched!",
                    Warning, Log_Section);
               This.Status := Idle;
               This.Out_Agent.Clear_Tasks;
            else
               if not This.Out_Agent.Has_Tasks then
                  --  Our daisy-chained executor has finished with it:
                  Done        := True;
                  This.Status := Idle;
                  This.Job.Clear;
                  Log ("Daisy-chained task completed",
                       Informative, Log_Section);
               end if;
            end if;
      end case;
   end Execute;

   ----------
   -- Idle --
   ----------

   procedure Idle (This : in out Object) is
      use Sancta.Types.Operations;
   begin
--        Log ("idling", Always);
      This.Out_Agent.Clear_Tasks;
      This.Status := Idle;

      if not This.Stopped then
         This.Stopped := True;
         This.Output (Provides_Goal,
                      This.Input (Requires_Pose));
         This.Output (Provides_Velo,
                      Types.Pose'(Pose => Sancta.Types.Origin));
      end if;
   end Idle;

end Sancta.Component.Executor_Posed;
