with Sancta.Map.Utils;

package body Sancta.Ctree.Tree_Navigator.Partial_Draw is

   ------------
   -- Create --
   ------------

   function Create
     (Nav  : access constant Tree_Navigator.Object'Class;
      Jobs : Tc.Lists.List)
      return Object
   is
   begin
      return This : Object (Nav) do
         This.Jobs := Jobs;
      end return;
   end Create;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This :        Object;
      Into : in out Agpl.Drawing.Drawer'Class)
   is
      use Sancta.Map.Utils;
      use Tc.Lists;
      procedure Draw (I : Cursor) is
      begin
         New_Path_Drawable (This.Nav.Branch (Element (I))).Draw (Into);
      end Draw;
   begin
      This.Jobs.Iterate (Draw'Access);
   end Draw;

end Sancta.Ctree.Tree_Navigator.Partial_Draw;
