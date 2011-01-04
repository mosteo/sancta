with Sancta.Player.Draw;

package body Sancta.Ctree.Step.Draw_Plan is

   -------------
   -- Process --
   -------------

   procedure Process
     (This : in out Object;
      Ctx  : in out Context.Object)
   is
      pragma Unreferenced (This);
   begin
      Sancta.Player.Draw.Draw_Plan (Ctx.Draw, Ctx.Plan);
   end Process;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Sancta.Ctree.Draw_Plan'Img,
                        new Object);
   end Register;

end Sancta.Ctree.Step.Draw_Plan;
