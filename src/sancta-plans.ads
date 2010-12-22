with Ada.Finalization,
     Agpl.Containers.Unbounded_Trees,
     Agpl.Generic_Handle;

package Sancta.Plans is

   --  New, updated, improved, plan management.
   --  Basically an abstraction on a tree.

   pragma preelaborate;

   type Plan is tagged limited private;

   type Node is abstract tagged null record;

   type Cursor is tagged private;
   --  Used to navigate a plan

private

   package Node_Trees is new Agpl.Containers.Unbounded_Trees (Node'Class,
                                                              Positive);

   package Cursor_Handle is new Agpl.Generic_Handle
     (Node_Trees.Cursor, Node_Trees."=");

   type Plan is new Ada.Finalization.Limited_Controlled with record
      Tree : Node_Trees.Tree;
   end record;

   procedure Initialize (This : in out Plan) is null;

   type Cursor is new Cursor_Handle.Object with null record;

   type Portable_Plan is null record;

end Sancta.Plans;
