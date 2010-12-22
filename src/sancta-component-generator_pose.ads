--  Statically provides a value supplied as option

with Sancta.Component.Generator;

package Sancta.Component.Generator_Pose is

   Log_Section : constant String := "sancta.component.generator_pose";

   Name        : aliased constant String := "generator_pose";

   Provides_Value : constant Internal_Key := "value";
   Option_Value   : constant Option_Attr  := "value";
   --  Format is there space-separated values: "X Y A"

   procedure Register;

private

   type Object is new Generator.Object with null record;

   overriding
   function To_Data (This : Object; Val : String) return Data'Class;

   function Create (Config : Comp_Config)
                    return   Component.Object_Access;

end Sancta.Component.Generator_Pose;
