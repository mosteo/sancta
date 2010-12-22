--  Sample empty component to save-as when creating new ones.

with Sancta.Component.Root;

package Sancta.Component.Skeleton is

   Log_Section : constant String := "sancta.component.skeleton";

   Name : aliased constant Component_Name := "skeleton";

   Requires_Whatever : constant Internal_Key := "whatever";
   Provides_Whatever : constant Internal_Key := "whatever";

   Option            : constant Option_Attr  := "option";

   procedure Register;

private

   type Object is new Root.Object with null record;

--     function Create (Config : Comp_Config)
--                      return   Component.Object_Access;

--     function Create (Config : Comp_Config;
--                      Env    : Environment.Object)
--                      return   Component.Object_Access;

--     overriding
--     procedure Key_Stored (This  : in out Object;
--                           Key   : in     Internal_Key;
--                           Value : in     Data'Class);

--     overriding
--     procedure Run (This : in out Object;
--                    Next :    out Ada.Calendar.Time);

--     overriding
--     procedure Stop (This : in out Object);

end Sancta.Component.Skeleton;
