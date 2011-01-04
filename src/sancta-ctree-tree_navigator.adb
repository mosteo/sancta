with Sancta.Map.Utils;

package body Sancta.Ctree.Tree_Navigator is

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This :        Object'Class;
      Dest : in out Agpl.Drawing.Drawer'Class)
   is
      use Tc.Lists;
      procedure Draw (I : Cursor) is
      begin
         Sancta.Map.Utils.Draw_Path (This.Branch (Element (I)), Dest);
      end Draw;
   begin
      This.Get_Tasks.Iterate (Draw'Access);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Object;
                   Dest : in out Agpl.Drawing.Drawer'Class) is
   begin
      This.Draw_Classwide (Dest);
   end Draw;

end Sancta.Ctree.Tree_Navigator;
