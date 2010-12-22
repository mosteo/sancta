with Sancta.Debug;

with Agpl.Generic_Handle;

package body Sancta.Tasks.Explore_Edge is

   package Cost_Handles is
     new Agpl.Generic_Handle (Pose_Graphs.Cost_Matrix, Pose_Graphs."=");
   use Cost_Handles;

   Cost : Cost_Handles.Object;

   ------------
   -- Create --
   ------------

   function Create (From, To : in Pose_Graphs.Vertex_Index)
                    return        Object
   is
   begin
      return (Sancta.Tasks.Compound.Object with
              V1 => From,
              V2 => To);
   end Create;

   --------------
   -- Get_Cost --
   --------------

   function Get_Cost (From,
                      To      : in Pose_Graphs.Vertex_Index)
                      return       Sancta.Costs
   is
   begin
      if not Cost.Is_Valid then
         Cost.Set (Pose_Graphs.Get_Costs (Graph));
      end if;

      return Sancta.Costs (Ref (Cost).Get (From, To));
   end Get_Cost;

   --------------
   -- Get_From --
   --------------

   function Get_From (This : in Object) return Pose_Graphs.Vertex_Index is
   begin
      return This.V1;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To (This : in Object) return Pose_Graphs.Vertex_Index is
   begin
      return This.V2;
   end Get_To;

   --------------
   -- Get_Pose --
   --------------

   function Get_Pose (V : in Pose_Graphs.Vertex_Index) return Types.Pose
   is
   begin
      return Graph.Get_Vertex (V).Data;
   end Get_Pose;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Object) return String is
      P1 : Types.Pose renames Graph.Get_Vertex (This.V1).Data;
      P2 : Types.Pose renames Graph.Get_Vertex (This.V2).Data;
   begin
      return "Explore edge " & Debug.To_String (P1) & " <-?-> " &
                               Debug.To_String (P2);
   end To_String;

end Sancta.Tasks.Explore_Edge;
