with Sancta.Debug;
with Sancta.Types.Real_Math; use Sancta.Types.Real_Math;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Tasks.Explore_Directed_Segment is

   use type Types.Real;

   ------------
   -- Create --
   ------------

   function Create (From, To : in Types.Pose) return Object is
      R : Object;
   begin
      R.Ini_Pose := From;
      R.Fin_Pose := To;

      declare
         Ang : constant Types.Real := Arctan (To.Y - From.Y, To.X - From.X);
      begin
         R.Ini_Pose.A := Types.To_Angle (Ang);
         R.Fin_Pose.A := R.Ini_Pose.A;
      end;

--      Log ("Created task Expl_Dir_Seg, ID:" & R.Get_Id'Img, Always);

      return R;
   end Create;

   ----------
   -- Flip --
   ----------

   function Flip (This : in Object) return Object is
   begin
      return Create (From => This.Get_To, To => This.Get_From);
   end Flip;

   --------------
   -- Get_From --
   --------------

   function Get_From (This : in Object) return Types.Pose is
   begin
      return This.Ini_Pose;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To (This : in Object) return Types.Pose is
   begin
      return This.Fin_Pose;
   end Get_To;

   ----------------
   -- On_Segment --
   ----------------

   function On_Segment (This : in Object) return Boolean is
   begin
      return This.On_Segment;
   end On_Segment;

   procedure Set_On_Segment (This : in out Object; On : in Boolean := True) is
   begin
      This.On_Segment := On;
   end Set_On_Segment;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
   begin
      return "Explore segment " & Debug.To_String (This.Ini_Pose) & " --> " &
                                  Debug.To_String (This.Fin_Pose);
   end To_String;

end Sancta.Tasks.Explore_Directed_Segment;
