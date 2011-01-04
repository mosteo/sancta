with Sancta.Ctree.Component.Ctree_Draw,
     Sancta.Ctree.Component.Ctree_Preorder,
     Sancta.Ctree.Component.Ctree_Signal_Distance,
     Sancta.Ctree.Component.Ctree_Signal_Olsr,
--       Sancta.Ctree.Component.Ctree_Signal_Rtwmp,
     Sancta.Ctree.Component.Ctree_Signal_Tunnel,
     Sancta.Ctree.Component.Ctree_Single,
     Sancta.Ctree.Component.Single_Distributed,
     Sancta.Ctree.Component.Ctree_Tasks_Order,
     Sancta.Ctree.Component.Distance_Links,
     Sancta.Ctree.Component.Hack,
     Sancta.Ctree.Component.Tree_Bitmap,
     Sancta.Ctree.Component.Tree_Ordered,

     Sancta.Component.Include;

use Sancta;

package body Sancta.Ctree.Components is

   package Comp renames Sancta.Ctree.Component;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Comp.Ctree_Draw.Register;
      Comp.Ctree_Preorder.Register;
      Comp.Ctree_Signal_Distance.Register;
      Comp.Ctree_Signal_Olsr.Register;
--        Comp.Ctree_Signal_Rtwmp.Register;
      Comp.Ctree_Signal_Tunnel.Register;
      Comp.Ctree_Single.Register;
      Comp.Single_Distributed.Register;
      Comp.Ctree_Tasks_Order.Register;
      Comp.Distance_Links.Register;
      Comp.Hack.Register;
      Comp.Tree_Bitmap.Register;
      Comp.Tree_Ordered.Register;

      Sancta.Component.Include.Register;
   end Register;

end Sancta.Ctree.Components;
