with Sancta.Map.Bitmap;
with Sancta.Map.Wavefront; pragma Elaborate_All (Sancta.Map.Wavefront);

function Sancta.Map.Bitmap_Wavefront_8 is new
Sancta.Map.Wavefront (Map.Bitmap.Vicinity_8,
                      Map.Bitmap.Get_Cost);
