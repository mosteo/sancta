with Agpl.Drawing;

package Sancta.Ctree.Tree_Navigator.Partial_Draw is

   type Object (Nav : access constant Tree_Navigator.Object'Class) is
     new Agpl.Drawing.Drawable with private;

   function Create (Nav  : access constant Tree_Navigator.Object'Class;
                    Jobs : Tc.Lists.List) return Object;
   --  Jobs are the one we want drawn.

   procedure Draw (This :        Object;
                   Into : in out Agpl.Drawing.Drawer'Class);

private

   type Object (Nav : access constant Tree_Navigator.Object'Class) is
     new Agpl.Drawing.Drawable with record
      Jobs : Tc.Lists.List;
   end record;

end Sancta.Ctree.Tree_Navigator.Partial_Draw;
