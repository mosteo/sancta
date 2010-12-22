with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Tasks.Speed_Driving,
     Sancta.Tasks.Wait_For_Orders,
     Sancta.Types;

package body Sancta.Component.Executor_Manual_Driving is

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
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
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

      procedure Do_Speed_Driving (Job : in out Tasks.Speed_Driving.Object) is
         T : Tasks.Speed_Driving.Object renames Job;
      begin
         if not T.Is_Started then
            T.Mark_Started;
         end if;

         --  Check finished
         if T.Finished then
            Done := True;
         else
            This.Output (Provides_Velo, Types.Pose'(Pose => T.Velocity));
         end if;
      end Do_Speed_Driving;

      procedure Do_Wait_For_Orders is
      begin
         This.Output (Provides_Goal, This.Input (Requires_Pose));
         This.Output (Provides_Velo,
                   Types.Pose'(Pose => Sancta.Types.Origin));
         Done := False;
      end Do_Wait_For_Orders;

   begin
      Done := False;
      if Job in Tasks.Speed_Driving.Object then
         This.Stopped := False;
         Do_Speed_Driving (Tasks.Speed_Driving.Object (Job));
      elsif Job in Tasks.Wait_For_Orders.Object then
         if not This.Stopped then
            Do_Wait_For_Orders;
            This.Stopped := True;
         end if;
      else
         This.Stopped := False;
      end if;
   end Execute;

end Sancta.Component.Executor_Manual_Driving;
