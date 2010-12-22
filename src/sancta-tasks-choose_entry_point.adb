package body Sancta.Tasks.Choose_Entry_Point is

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
     (Candidates : in Sancta.Types.Pose_Array)
      return Object
   is
   begin
      return (Sancta.Tasks.Compound.Object with Candidates =>
                Types.Pose_Vector.To_Vector
                  (Types.Pose_Vector.Item_Array (Candidates)));
   end Create;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
      pragma Unreferenced (This);
   begin
      return "Choose entry point.";
   end To_String;

end Sancta.Tasks.Choose_Entry_Point;
