with Sancta.Types;

with Sancta.Tasks.Primitive;

package Sancta.Tasks.Explore_Directed_Segment is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Primitive.Object with private;
   --  The robot can't deviate from this line.
   --  The From->to sense must be observed.

   function Create (From, To : in Types.Pose) return Object;
   --  This will automatically adjust the From and To angles in the sense of
   --  movement.

   function Get_From (This : in Object) return Types.Pose;

   not overriding
   function Get_To (This : in Object) return Types.Pose;

   function To_String (This : Object) return String;
   function Image (This : Object) return String renames To_String;

   ----------------------------
   -- Task execution helpers --
   ----------------------------

   function Flip (This : in Object) return Object;
   --  Gives a task with the opposite sense.

   function On_Segment (This : in Object) return Boolean;
   --  Says if we're already over the segment to walk.

   procedure Set_On_Segment (This : in out Object; On : in Boolean := True);

private

   type Object is new Sancta.Tasks.Primitive.Object with record
      Ini_Pose   : Types.Pose;
      Fin_Pose   : Types.Pose;

      On_Segment : Boolean := False;
   end record;

end Sancta.Tasks.Explore_Directed_Segment;
