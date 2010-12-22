with Agpl.Drawing;
with Sancta.Containers; use Sancta.Containers;
with Sancta.Tasks.Goto_Pose;

package Sancta.Map.Utils is

   Log_Section : constant String := "sancta.map.utils";

   function Random_Location
     (M     : Object'Class;
      Valid : access function (O : Observation'Class) return Boolean)
      return Location'Class;
   --  Get a random location, after approving it (e.g. for freeness)
   --  It's slow, since there's no direct access to locations

   function To_Task (L : Location'Class) return Sancta.Tasks.Goto_Pose.Object;

   function To_List (V : Location_Vectors.Vector) return Tc.Lists.List;

   function Remove_Loops (P : Path) return Path;

   procedure Draw_Path (P :        Path;
                        D : in out Agpl.Drawing.Drawer'Class);

   procedure Draw_Pose (P : Types.Pose;
                        D : in out Agpl.Drawing.Drawer'Class;
                        Size : Float := 0.5);

   type Path_Drawable is new Agpl.Drawing.Drawable with private;

   procedure Draw (This :        Path_Drawable;
                   Into : in out Agpl.Drawing.Drawer'Class);

   function New_Path_Drawable (P : Path) return Path_Drawable;

private

   type Path_Drawable is new Agpl.Drawing.Drawable with record
      P : Path;
   end record;

end Sancta.Map.Utils;
