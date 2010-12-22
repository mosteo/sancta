with Agpl.Drawing.Figures;
with Sancta.Types.Transformations;

package body Sancta.Component.Types is

-------------
-- Boolean --
-------------

   function Boolean (Value : Standard.Boolean) return Bool is
   begin
      return Bool'(Value => Value);
   end Boolean;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (This :        Map_Data;
      D    : in out Agpl.Drawing.Drawer'Class)
   is
   begin
      This.Map.Ref.Draw (D);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Robot_Pose;
                   D    : in out Agpl.Drawing.Drawer'Class)
   is
      use type Sancta.Types.Real;
   begin
      D.Set_Color ((255, 0, 0), 0);
      Agpl.Drawing.Figures.Robot (+This.Pose.X,
                                  +This.Pose.Y,
                                  Float (This.Pose.A)).Draw (D);
   end Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Posed_Scan;
                   D    : in out Agpl.Drawing.Drawer'Class)
   is
      use Sancta.Types;
      use Sancta.Types.Transformations;
      use Sancta.Types.Transformations.Real_Transf;

      Semigap : constant Angle :=
                  (This.Fin - This.Ini) / Angle (This.Length - 1) / 2.0;

      T : constant Transformation :=
            Get_Composition (+This.Robot_Pose) *
            Get_Composition (+This.Laser_Pose_On_Robot);
   begin
      D.Set_Color ((0, 0, 255), 0);

      for I in This.Samples'Range loop
         declare
            PP1 : constant Sancta.Types.Pose := Polar_To_Cart
              (This.Samples (I).A - Semigap,
               This.Samples (I).D);

            PP2 : constant Sancta.Types.Pose := Polar_To_Cart
              (This.Samples (I).A + Semigap,
               This.Samples (I).D);

            P1  : constant Sancta.Types.Pose :=
                    + (T * (+PP1.X, +PP1.Y, 0.0, 1.0));

            P2  : constant Sancta.Types.Pose :=
                   + (T * (+PP2.X, +PP2.Y, 0.0, 1.0));
         begin
            D.Draw_Line (+P1.X, +P1.Y, +P2.X, +P2.Y);
         end;
      end loop;
   end Draw;

end Sancta.Component.Types;
