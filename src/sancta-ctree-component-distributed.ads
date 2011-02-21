with Sancta.Ctree.Distributed;
with Sancta.Component.Root; use Sancta.Component;
with Sancta.Types;

private with Ada.Calendar;
private with Agpl.Chronos;
private with Agpl.Tasking.Period;
private with Sancta.Component.Environment;

package Sancta.Ctree.Component.Distributed is

   Log_Section : constant String := "sancta.ctree.component.distributed";

   Name : aliased constant Component_Name := "ctree_distributed";

   Requires_Link       : constant Internal_Key := "link";
   Requires_Pose       : constant Internal_Key := "pose";
   Requires_Raw_Signal : constant Internal_Key := "raw_signal"; -- nctypes.signal
   Requires_Tasks      : constant Internal_Key := "tasks";
   Requires_Map        : constant Internal_Key := "map";
   Requires_Navigator  : constant Internal_Key := "navigator";
   Requires_Drawer     : constant Internal_Key := "drawer"; -- optional

   Provides_Started       : constant Internal_Key := "in_command"; -- ctypes.bool
   --  Set to true/false when Go/Stop/Park/Cancel determines
   Provides_Pending_Tasks : constant Internal_Key := "pending_tasks";
   Provides_Goal          : constant Internal_Key := "goal";
   Provides_Velo          : constant Internal_Key := "velo";
   Provides_Status_Draw   : constant Internal_Key := "status_draw";
   Provides_Finished      : constant Internal_Key := "finished";
   Provides_Node_Status   : constant Internal_Key := "node_status";
   --  Encapsulated info about the local node
   Provides_Density_Draw  : constant Internal_Key := "density_draw";
   --  Parceled drawable with density of samples per location
   Provides_Quality_Draw  : constant Internal_Key := "quality_draw";
   --  Parceled drawable with quality pairs

   Opt_Base_Id : constant Option_Attr := "base_id"; -- Id of base node

   Opt_Base_Pose : constant Option_Attr := "base_pose"; -- x y z

   Opt_Here    : constant Option_Attr := "here"; -- min dist among stopped bots
   Opt_Near    : constant Option_Attr := "near"; -- min dist among moving bots

   Opt_Signal_Threshold : constant Option_Attr := "signal_threshold";

   Opt_Loc_Dist    : constant Option_Attr := "loc_dist";
   Opt_Target_Dist : constant Option_Attr := "target_dist";

   Opt_Rot_Vel : constant Option_Attr := "rot_vel";
   Def_Rot_Vel : constant Sancta.Types.Angle := 0.3;
   Opt_Period  : constant Option_Attr := "period";
   Def_Period  : constant Duration    := 0.2;
   Opt_Channel : constant Option_Attr := "channel"; -- Network.Channel

   procedure Register;

private

   use Sancta;

   type Inner_Access is access all Ctree.Distributed.Object;

   type Object is new Root.Object with record
      Period : Agpl.Tasking.Period.Object :=
                 Agpl.Tasking.Period.Create (Def_Period);

      Timer_Density : Agpl.Chronos.Object;

      Mover  : Inner_Access;
   end record;

   function Create (Config : Comp_Config;
                    Env    : Environment.Object)
                    return   Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Ctree.Component.Distributed;
