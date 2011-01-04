with Sancta.Ctree.Context;

package Sancta.Ctree.Step.Draw_Plan is

   type Object is new Step.Object with null record;
   type Object_Access is access all Object'Class;

   procedure Process (This : in out Object;
                      Ctx  : in out Context.Object);

   procedure Register;

end Sancta.Ctree.Step.Draw_Plan;
