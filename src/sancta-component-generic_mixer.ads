with Sancta.Component.Root;

with Ada.Finalization;

package Sancta.Component.Generic_Mixer is

   --  Takes two inputs and spits some function of them.
   --  If one of the two doesn't exist when the other is being updated,
   --    the one existing is passed along.
   --  Subscription is automatically created here.

   Name       : aliased constant Component_Name := "y_or_else_x";

   Requires_X : constant Internal_Key := "x";
   Requires_Y : constant Internal_Key := "y";
   --  The two inputs

   Provides_Z : constant Internal_Key := "z";

   type Object is new Root.Object with private;

   type Object_Access is access all Object'Class;

   not overriding
   function Mix (This : not null access Object;
                 X, Y :                 Data'Class) return Data'Class;
   --  Override with your desired mixing function.
   --  It is called each time that x, y are stored
   --  This default returns Y.

   function Create (Config : in Agpl.Xml.Node)
                    return      Component.Object_Access;

   procedure Register;

private

   type Object_Preparer (Parent : access Object) is limited new
     Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out Object_Preparer);

   type Object is new Root.Object with record
      Preparer : Object_Preparer (Object'Access);
   end record;

   overriding
   procedure Key_Stored (This  : in out Object;
                         Key   : in     Internal_Key;
                         Value : in     Data'Class);

end Sancta.Component.Generic_Mixer;
