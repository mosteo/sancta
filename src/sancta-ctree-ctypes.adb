with Agpl.Constants;

package body Sancta.Ctree.CTypes is

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This :        Tree_Nav;
      Dest : in out Agpl.Drawing.Drawer'Class)
   is
      use Agpl.Constants;
   begin
      Dest.Set_Color (Navy, Alpha_Opaque);
      This.Tree.Ref.Draw (Dest);
   end Draw;

end Sancta.Ctree.CTypes;
