--  Statically provides a value supplied as option

with Sancta.Component.Root;

private with Ada.Finalization;

package Sancta.Component.Generator is

   Log_Section : constant String := "sancta.component.generator";

   Provides_Value : constant Internal_Key := "value";
   Option_Value   : constant Option_Attr  := "value";

   type Object is abstract new Root.Object with private;

   not overriding
   function To_Data (This : Object; Val : String) return Data'Class is abstract;
   --  Override with particular instances.

private

   type Object_Initer (Parent : access Object'Class) is
     new Ada.Finalization.Controlled with null record;

   procedure Initialize (This : in out Object_Initer);

   type Object is abstract new Root.Object with record
      Initer : Object_Initer (Object'Access);
   end record;

end Sancta.Component.Generator;
