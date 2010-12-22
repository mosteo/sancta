with Sancta.Map.Bitmap;

package Sancta.Map.Qtree.Builder is

   Default_Terrain : Terrains := Obstacle;
   --  Some maps will be bounded and some not.
   --  Thread-unsafe, of course.

   procedure From_Png (This        : in out Object;
                       File        :        String;
                       Xmin, Xmax  :        X_Real;
                       Ymin, Ymax  :        Y_Real;
                       Buggy_Stage :        Boolean := True;
                       Id          :        String  := Default_Map_Id);
   --  Cell dimensions must have been set previously or defaults will be used.
   --  The id is for serialization of locations across network.

   procedure From_Bitmap (This        : in out Object;
                          Bmp         :        Bitmap.Object;
                          Xmin, Xmax  :        X_Real;
                          Ymin, Ymax  :        Y_Real;
                          Id          :        String := Default_Map_Id);

end Sancta.Map.Qtree.Builder;
