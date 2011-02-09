--  with Ada.Text_IO; use Ada.Text_IO;
--  with Agpl.Strings; use Agpl.Strings;
with Sancta.Ctree.Component.Nctypes;
with Sancta.Component.Ctypes;
with Sancta.Component.Factory;
with Sancta.Component.Network;
with Sancta.Convert;
with Sancta.Network;

package body Sancta.Ctree.Component.Distributed is

   type Object_Access is access all Object;

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
     (Config : Comp_Config;
      Env    : Environment.Object)
      return Sancta.Component.Object_Access
   is
      pragma Unreferenced (Env);
      use Ctree.Distributed;
      This : constant Object_Access := new Object (Name'Access, Config);
      Conf :          Config_Type;
   begin
      if This.Exists (Opt_Period) then
         This.Period.Set_Period (Duration'Value (This.Option (Opt_Period)));
      end if;

      This.Mover := new Ctree.Distributed.Object
        (Link => Sancta.Component.Network.Network (This.Input (Requires_Link)).Link);

      if This.Exists (Opt_Rot_Vel) then
         Conf.Rot_Vel := Types.Angle'Value (This.Option (Opt_Rot_Vel));
      else
         Conf.Rot_Vel := Def_Rot_Vel;
      end if;

      Conf.Here_Dist_Threshold := Types.Real'Value (This.Option (Opt_Here));
      Conf.Near_Dist_Threshold := Types.Real'Value (This.Option (Opt_Near));
      Conf.Loc_Dist_Threshold  := Types.Real'Value (This.Option (Opt_Loc_Dist));
      Conf.Target_Dist_Threshold :=
        Types.Real'Value (This.Option (Opt_Target_Dist));
      Conf.Signal_Threshold :=
        Signal_Q'Value (This.Option (Opt_Signal_Threshold));

      This.Mover.Create
        (Config  => Conf,
         Base    => +This.Option (Opt_Base_Id),
         Base_Pose => Convert.To_Pose (This.Option (Opt_Base_Pose)),
         Map     => Ctypes.Map_Data (This.Input (Requires_Map)).Map,
         Channel => Sancta.Network.Value (This.Option (Opt_Channel)));

      This.Subscribe (Requires_Navigator);

      return Sancta.Component.Object_Access (This);
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
      use Ctree.Distributed;
   begin
      --  INPUTS
      if This.Exists (Requires_Pose) then
         This.Mover.Set_Pose (Ctypes.Pose (This.Input (Requires_Pose)).Pose);
      end if;

      if This.Provided_And_Exists (Requires_Raw_Signal) then
         This.Mover.Set_qualities
           (Nctypes.Signal (This.Input (Requires_Raw_Signal)).Links);
      end if;

      if This.Exists (Requires_Tasks) then
         This.Mover.Set_Ordered_Tasks
           (Ctypes.Task_List (This.Input (Requires_Tasks)).Tasks);
      end if;

      if This.Provided (Requires_Drawer) and then
         This.Exists   (Requires_Drawer)
      then
         This.Mover.Set_Drawer
           (Ctypes.Drawer (This.Input (Requires_Drawer)).Drawer);
      end if;

      --  STEP
      This.Mover.Step;

      --  OUTPUT
      if This.Mover.Is_Commanding_Robot then
         This.Output (Provides_Started, Ctypes.Bool'(Value => True));
      else
         This.Output (Provides_Started, Ctypes.Bool'(Value => False));
      end if;

      if This.Mover.Is_Commanding_Robot then
         declare
            Action : constant Robot_Orders := This.Mover.Orders;
         begin
            case Action.Action is
            when Wait =>
               This.Output (Provides_Velo, Ctypes.Pose'(Pose => Types.Null_Pose));
            when Move =>
               This.Output (Provides_Goal, Ctypes.Pose'(Pose => Action.Goal));
            when Turn =>
               This.Output (Provides_Velo, Ctypes.Pose'(Pose => Action.Velo));
            end case;
         end;
      end if;

      if This.Provided (Provides_Status_Draw) then
         This.Output (Provides_Status_Draw, This.Mover.Get_Draw_Status);
      end if;

      if This.Provided (Provides_Node_Status) then
         This.Output (Provides_Node_Status, This.Mover.Get_Node_Status);
      end if;

      if This.Exists (Requires_Tasks) then
         --  We don't want to overwrite with our empty tasks if they're not yet
         --  ready.
         This.Output (Provides_Pending_Tasks,
                      Ctypes.Task_List'(Tasks => This.Mover.Pending_Tasks));
      end if;

      if This.Provided (Provides_Density_Draw) and then This.Timer_Density.Elapsed >= 1.0 then
         This.Output (Provides_Density_Draw,
                      Wrap (This.Mover.Density_Map, "Density", This.Get_Id));
         This.Timer_Density.Reset;
      end if;

--        if This.Mover.Status = Mission_Completed then
--           This.Output (Provides_Finished, Ctypes.Bool'(Value => True));
--        end if;
--  Better we manually kill it

      This.Period.Next (Next);
   end Run;

   ----------------
   -- Key_Stored --
   ----------------

   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class)
   is
   begin
      if Key = Requires_Navigator then
         This.Mover.Set_Navigator (Nctypes.Tree_Nav (Value).Tree);
      end if;
   end Key_Stored;

end Sancta.Ctree.Component.Distributed;
