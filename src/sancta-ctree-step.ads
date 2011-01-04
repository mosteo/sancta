with Sancta.Ctree.Context;
with Sancta.Ctree.Factory;

package Sancta.Ctree.Step is

   type Object is abstract new Factory.Object with null record;
   type Object_Access is access all Object'Class;

   procedure Process (This : in out Object;
                      Ctx  : in out Context.Object) is abstract;

end Sancta.Ctree.Step;
