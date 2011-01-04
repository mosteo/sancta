with Sancta.Component.Root; use Sancta.Component;

private with Ada.Calendar;
private with Agpl.Containers.String_String_Maps;
private with Agpl.Tasking.Period;
private with Sancta.Ctree;
private with Sancta.Component.Environment;

package Sancta.Ctree.Component.Ctree_Signal_Olsr is

   Log_Section : constant String := "Sancta.Ctree.Component.ctree_signal_olsr";
   Det_Section : constant String := Log_Section & ".detail";

   Name : aliased constant Component_Name := "ctree_signal_olsr";

   --  This is special in which it needs all name/IP equivalences again.
   --  These are given as:
   --  (Note no port!)
   --           <node id="Ari" address="192.168.1.1"/>
   --           <node id="Ben" address="192.168.1.2"/>
   --           <node id="Ced" address="192.168.1.3"/>
   --           <node id="Dan" address="192.168.1.4"/>

   Provides_Signal    : constant Internal_Key := "signal";
   --  nctypes.signal

   Opt_Txtport : constant Option_Attr := "txtport";
   Def_Txtport : constant Natural     := 8899;
   --  Port in which the olsrd txtinfo plugin is listening at localhost.

   Opt_Period  : constant Option_Attr := "period";
   Def_Period  : constant Duration    := 0.5;

   procedure Register;

private

   type Object;

   use Sancta;

   type Object is new Root.Object with record
      Id         : Node_Id; -- ourselves

      Links      : Ctree.Id_Q_Maps.Map;
      Zero_Links : Ctree.Id_Q_Maps.Map; -- Used to reset to 0 all links

      Port       : Natural := Def_Txtport;
      Ip_Id      : Agpl.Containers.String_String_Maps.Map;

      Period     : Agpl.Tasking.Period.Object :=
                     Agpl.Tasking.Period.Create (Def_Period);
   end record;

   function Create (Config : Comp_Config;
                    Env    : Environment.Object)
                    return   Sancta.Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

   not overriding
   procedure Add_Link (This     : in out Object;
                       Other_Id :        Node_Id;
                       Other_Q  :        Signal_Q);

end Sancta.Ctree.Component.Ctree_Signal_Olsr;
