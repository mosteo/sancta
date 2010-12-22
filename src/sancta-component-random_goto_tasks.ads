--  Create random tasks

with Sancta.Component.Root,
     Sancta.Types;

package Sancta.Component.Random_Goto_Tasks is

   Log_Section : constant String := "sancta.component.random_goto_tasks";

   Name : aliased constant Component_Name := "random_goto_tasks";

   Provides_Tasks : constant Internal_Key := "tasks";

   Option_Origin : constant Option_Attr := "origin";
   Default_Origin : constant Types.Pose := Types.Origin;

   Option_Range  : constant Option_Attr := "range";
   Default_Range : constant Float       := 32.0;

   Option_Random_Seed : constant Option_Attr := "random_seed";
   Default_Random_Seed : constant Integer    := 6976;

   Option_Amount : constant Option_Attr := "amount";
   Default_Amount : constant := 20;

   procedure Register;

private

   type Object is new Root.Object with null record;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

end Sancta.Component.random_Goto_Tasks;
