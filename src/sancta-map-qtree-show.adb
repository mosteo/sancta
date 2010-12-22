with Agpl.Constants;
with Sancta.Map.Utils;

package body Sancta.Map.Qtree.Show is

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Object;
                   Path :        Map.Path := Map.Empty_Path;
                   D    : in out Agpl.Drawing.Drawer'Class) is
   begin
      This.Draw (D);

   --  Path
      if not Path.Is_Empty then
         D.Set_Color (Agpl.Constants.Red, Agpl.Constants.Alpha_Opaque);
         Utils.Draw_Path (Path, D);
      end if;
   end Draw;

end Sancta.Map.Qtree.Show;
