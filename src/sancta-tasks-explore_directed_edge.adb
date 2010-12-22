with Sancta.Debug;

package body Sancta.Tasks.Explore_Directed_Edge is

   ------------
   -- Create --
   ------------

   function Create (From,
                    To      : in Tasks.Explore_Edge.Pose_Graphs.Vertex_Index)
                    return  Object
   is
   begin
      return (Sancta.Tasks.Primitive.Object with
              Ini         => From,
              Fin         => To,
              On_Segment => False);
   end Create;

   --------------
   -- Get_From --
   --------------

   function Get_From (This : in Object)
                      return    Tasks.Explore_Edge.Pose_Graphs.Vertex_Index is
   begin
      return This.Ini;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To (This : in Object)
                    return Tasks.Explore_Edge.Pose_Graphs.Vertex_Index is
   begin
      return This.Fin;
   end Get_To;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
   begin
      return "Explore edge " &
      Debug.To_String (Tasks.Explore_Edge.Get_Pose (This.Ini)) &
      " --> " &
      Debug.To_String (Tasks.Explore_Edge.Get_Pose (This.Fin));
   end To_String;

   ----------------------------
   -- Task execution helpers --
   ----------------------------

   ----------
   -- Flip --
   ----------

   function Flip (This : in Object) return Object is
   begin
      return Create (From => This.Ini,
                     To   => This.Fin);
   end Flip;

   ----------------
   -- On_Segment --
   ----------------

   function On_Segment (This : in Object) return Boolean is
   begin
      return This.On_Segment;
   end On_Segment;

   --------------------
   -- Set_On_Segment --
   --------------------

   procedure Set_On_Segment (This : in out Object; On : in Boolean := True) is
   begin
      This.On_Segment := On;
   end Set_On_Segment;

end Sancta.Tasks.Explore_Directed_Edge;
