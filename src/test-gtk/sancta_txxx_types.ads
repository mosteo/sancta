with Agpl.Gdk.Managed.Drawing_Area;
with Sancta.Map;
with Sancta.Map.Qtree;

use Sancta;

package Sancta_Txxx_types is

   use Agpl.Gdk;

   type Void_Draw is new Managed.Drawing_Area.Draw_Code with null record;
   procedure Execute (This : in out Void_Draw) is null;

end Sancta_Txxx_Types;
