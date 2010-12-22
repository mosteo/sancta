with Sancta.Component.Root;

package Sancta.Component.Merger is

   --  Outputs two inputs to a same output, by subscription.

   Name       : aliased constant Component_Name := "merger";

   Requires_X : constant Internal_Key := "x";
   Requires_Y : constant Internal_Key := "y";
   --  The two inputs

   Provides_Z : constant Internal_Key := "z";

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   procedure Register;

private

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   type Object is new Root.Object with null record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Merger;
