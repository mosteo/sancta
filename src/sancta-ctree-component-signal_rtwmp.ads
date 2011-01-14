with Sancta.Component.Root; use Sancta.Component;
with Sancta.Network.Layer.Rtwmp;

private with Ada.Calendar;
private with Agpl.Average_Queue;
private with Agpl.Tasking.Period;
private with Agpl.Trace.File;
private with Sancta.Component.Environment;

package Sancta.Ctree.Component.Signal_Rtwmp is

   Log_Section : constant String := "Sancta.Ctree.Component.ctree_signal_rtwmp";
   Det_Section : constant String := Log_Section & ".detail";

   Name : aliased constant Component_Name := "ctree_signal_rtwmp";

   Requires_Link      : constant Internal_Key := "link";
   --  Of Layer.Root'Class, must be in Rtwmp'Class.

   Provides_Raw_Signal    : constant Internal_Key := "raw_signal";
   Provides_Avg_Signal    : constant Internal_Key := "avg_signal";
   Provides_Med_Signal    : constant Internal_Key := "med_signal";
   --  nctypes.signal

   Provides_Raw_Full_Signal : constant Internal_Key := "raw_full_signal";
   --  nctypes.full_signal
   --  Both RTWMP and a simulated signal can be locally provide a full picture
   --  of the network; so there's no need to burden ourselves with propagating
   --  that stuff.

   Opt_Avg_Period  : constant Option_Attr := "avg_period";
   Def_Avg_Period  : constant Duration    := 3.0;
   --  Period for running average.

   Opt_Period      : constant Option_Attr := "period";
   Def_Period      : constant Duration    := 0.1;

   procedure Register;

private

   type Object;

   use Sancta;
   use Sancta.Network.Layer.Rtwmp;

   package Avg_Signal is new Agpl.Average_Queue (Signal_Quality);

   type Avg_Array is array (Rtwmp_Address range <>) of Avg_Signal.Object_Access;

   type Object is new Root.Object with record
      Id         : Node_Id; -- ourselves

      Link       : Network.Layer.Object_Access;
      Avgs       : access Avg_Array;

      First_Log  : Boolean := True;
      Logger     : Agpl.Trace.File.Object;

      Period     : Agpl.Tasking.Period.Object :=
                     Agpl.Tasking.Period.Create (Def_Period);
   end record;

   function Create (Config : Comp_Config;
                    Env    : Environment.Object)
                    return   Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Ctree.Component.Signal_Rtwmp;
