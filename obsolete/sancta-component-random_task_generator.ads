--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Root;
private with Sancta.Types;

private with Agpl.Chronos;

package Sancta.Component.Random_Task_Generator is

   pragma Elaborate_Body;

   Log_Section : constant String := "sancta.Component.random_task_generator";

   Plugin_Name : constant String := "random_task_generator";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   not overriding
   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   overriding
   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time);

private

   use type Sancta.Types.Real;

   type Object is new Root.Object with record
      Xmin, Ymin : Float      := -10.0;
      Xmax, Ymax : Float      :=  10.0;
      Length     : Float      :=   2.0;
      Delaymin, Delaymax      : Duration := 4.0;
      Next_Delay              : Duration;
      Timer                   : Agpl.Chronos.Object;
      Num_Tasks               : Natural  := 20;
   end record;

end Sancta.Component.Random_Task_Generator;
