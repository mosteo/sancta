with Sancta.Component.Root; use Sancta.Component;
with Sancta.Component.Ctypes;

private with Ada.Calendar;
private with Agpl.Average_Queue;
private with Agpl.Tasking.Period;
private with Sancta.Component.Factory;

generic
   with package Reals is new Ctypes.Reals (<>);
   Name : String;
package Sancta.Component.Smooth_Periodic is

   Log_Section : constant String := "sancta.component.smooth_periodic";
   Det_Section : constant String := Log_Section & ".detail";

   Requires_X  : constant Internal_Key := "x";
   --  of Reals.Real

   Provides_Y  : constant Internal_Key := "y";
   --  of Reals.Real

   Opt_Avg_Period  : constant Option_Attr := "smooth_period";
   Def_Avg_Period  : constant Duration    := 3.0;
   --  Period for running average.

   Opt_Period      : constant Option_Attr := "period";
   Def_Period      : constant Duration    := 0.1;

   type Kinds is (Average, Median);
   Opt_Kind        : constant Option_Attr := "kind";

   procedure Register;

private

   type Object;

   package Avg_Item is new Agpl.Average_Queue (Reals.Item);

   type Object is new Root.Object with record
      Avg        : Avg_Item.Object_Access;
      Period     : Agpl.Tasking.Period.Object :=
                     Agpl.Tasking.Period.Create (Def_Period);
   end record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

   Create_Ptr : constant Factory.Object_Creator := Create'Access;
   Name_Ptr   : constant access String := new String'(Name);

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

end Sancta.Component.Smooth_Periodic;
