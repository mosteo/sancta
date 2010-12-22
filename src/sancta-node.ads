with Ada.Finalization;

package Sancta.Node is

   --  Base class for simple nodes not needing anything fancy (e.g. for unit tests)
   --  Upon instantiation, it will launch itself

   type Object is tagged private;

   not overriding
   procedure Register_Components (This : in out Object) is null;
   --  No need to call it directly, will be called automatically during
   --  instantiation.

private

   type Object is new Ada.Finalization.Controlled with null record;

   overriding
   procedure Initialize (This : in out Object);

end Sancta.Node;
