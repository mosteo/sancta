with Agpl.Drawing.Figures;
with Sancta.Located_Agent,
     Sancta.Types.Operations;

package body Sancta.Tasks.Positioned is

   ------------
   -- Create --
   ------------

   function Create (Pose : Types.Pose) return Object'Class is
   begin
      return Object'(Sancta.Tasks.Primitive.Object with Pose);
   end Create;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (This : Object) return Types.Pose is
   begin
      return This.Pose;
   end Get_Pose;

   --------------
   -- Set_Pose --
   --------------

   procedure Set_Pose (This : in out Object; Pose : Types.Pose) is
   begin
      This.Pose := Pose;
   end Set_Pose;

   ---------------
   -- Completed --
   ---------------

   function Completed (This : Object;
                       Bot  : Agent.Object'Class) return Boolean
   is
      use type Types.Real;
      use Types.Operations;
   begin
      if Bot in Located_Agent.Object'Class then
         return
           Distance (This.Pose,
                     Located_Agent.Object'Class (Bot).Get_Pose) < 2.0;
      else
         return False;
      end if;
   end Completed;

   ----------
   -- Draw --
   ----------

   procedure Draw (This :        Object;
                   D    : in out Agpl.Drawing.Drawer'Class)
   is
      use type Types.Real;
   begin
      declare
         Fig : constant Agpl.Drawing.Drawable'Class :=
                 Drawing.Figures.Target (+This.Pose.X, +This.Pose.Y);
      begin
         Fig.Draw (D);
      end;
   end Draw;

end Sancta.Tasks.Positioned;
