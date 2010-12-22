with Sancta.Tasks.Explore_Edge;

with Sancta;
with Sancta.Tasks.Primitive;

package Sancta.Tasks.Explore_Directed_Edge is

   pragma Preelaborate;

   type Object is new Sancta.Tasks.Primitive.Object with private;
   --  The robot can't deviate from this line.
   --  The From->to sense must be observed.

   function Create (From,
                    To      : in Tasks.Explore_Edge.Pose_Graphs.Vertex_Index)
                    return  Object;
   --  This will automatically adjust the From and To angles in the sense of
   --  movement.

   function Get_Cost (From,
                      To      : in Tasks.Explore_Edge.Pose_Graphs.Vertex_Index)
                      return Sancta.Costs renames Tasks.Explore_Edge.Get_Cost;

   function Get_From (This : in Object)
                      return    Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;

   function Get_To (This : in Object)
                    return Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;

   function To_String (This : Object) return String;

   ----------------------------
   -- Task execution helpers --
   ----------------------------

   function Flip (This : in Object) return Object;
   --  Gives a task with the opposite sense.

   function On_Segment (This : in Object) return Boolean;
   --  Says if we're already over the segment to walk.

   procedure Set_On_Segment (This : in out Object; On : in Boolean := True);

--     function Input(Stream : access Ada.Streams.Root_Stream_Type'Class)
--       return Object;
--
--     procedure Output (Stream : access Ada.Streams.Root_Stream_Type'Class;
--                       Item   : in     Object);

private

   pragma Inline (Create, Get_From, Get_To, Flip, On_Segment, Set_On_Segment);

   type Object is new Sancta.Tasks.Primitive.Object with record
      Ini        : Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;
      Fin        : Tasks.Explore_Edge.Pose_Graphs.Vertex_Index;

      On_Segment : Boolean := False;
   end record;

end Sancta.Tasks.Explore_Directed_Edge;
