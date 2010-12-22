with Sancta.Debug2;

package body Sancta.Tasks.Entry_Point is

   ----------------
   -- Candidates --
   ----------------

   function Candidates (This : in Object) return Sancta.Types.Pose_Array
   is
   begin
      return Types.Pose_Array (This.Candidates.Vector
                                 (This.Candidates.First .. This.Candidates.Last));
   end Candidates;

   ------------
   -- Create --
   ------------

   function Create
     (Pose       : in Sancta.Types.Pose;
      Candidates : in Sancta.Types.Pose_Array)
      return Object
   is
      This : Object;
   begin
--      Log ("Created task Entry_Point, ID:" & This.Get_Id'Img, Always);
      This.Set_Pose (Pose);
      This.Candidates := Types.Pose_Vector.To_Vector
        (Types.Pose_Vector.Item_Array (Candidates));
      return This;
   end Create;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
   begin
      return "Entry at " & Debug2.To_String (This.Pose);
   end To_String;

end Sancta.Tasks.Entry_Point;
