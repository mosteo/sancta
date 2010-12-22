with Agpl.Reflection.Booleans;
with Agpl.Reflection.Durations;

package Sancta.Options is

   package Reflect renames Agpl.Reflection;

   --  GLOBAL OPTIONS THAT APPLY ACROSS SANCTA
   --  Defaults are safe, unefficient

   --  These defaults can be modified from sancta.xml via 'global' elements

   Concurrent_Events        : Reflect.Booleans.Object :=
     Reflect.Booleans.Value (("Concurrent_Events"),
                             False);
   --  When false, all plugin calls occur secuentially in the same thread.
   --  When true, a new worker task is created for each call.
   --  Heavier and may break programs that rely on the old behavior, but at
   --    the same time prevents a plugin stopping all of them...
   --  Component creation is, in any case, executed sequentially
   --  This is the default behavior for plugins that don't override it.
   --  They should know better...

   Protected_Events         : Reflect.Booleans.Object :=
     Reflect.Booleans.Value (("Protected_Events"),
                             True);

   Max_Component_Creation_Time         : Reflect.Durations.Object :=
     Reflect.Durations.Value (("Max_Component_Creation_Time"),
                              5.0);
   --  Time allowed for first Create call

   Max_Component_Running_Time_Per_Call : aliased Reflect.Durations.Object :=
     Reflect.Durations.Value (("Max_Component_Running_Time_Per_Call"),
                              1.0);
   --  Plugin calls exceding this time will be aborted!

   Max_Component_On_Demand_Wait_Period : aliased Reflect.Durations.Object :=
     Reflect.Durations.Value (("Max_Component_On_Demand_Wait_Period"),
                              5.0);
   --  Time waiting for requires to appear before complaining

end Sancta.Options;


