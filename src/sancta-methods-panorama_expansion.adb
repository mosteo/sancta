with Sancta.Tasks.Panorama_At_Coords;
with Sancta.Tasks.Snapshot_At_Pose;

package body Sancta.Methods.Panorama_Expansion is

   -----------
   -- Apply --
   -----------

   function Apply
     (This : in Object;
      That : in Sancta.Tasks.Object'Class)
      return Result
   is
      Nodes : Sancta.Plan_Node.Node_Lists.List;
      use Sancta.Plan_Node.Node_Lists;
      use type Types.Real;
   begin
      --  Return an empty result if the tasks is other than Panorama_At_Coords
      if That not in Tasks.Panorama_At_Coords.Object'Class then
         return null;
      end if;

      declare
         Coords : Types.Point renames
           Tasks.Panorama_At_Coords.Object (That).Coords;
         Pos    : Tasks.Snapshot_At_Pose.Object;
         use type Types.Angle;
      begin
         Pos.Pose.X := Coords.X;
         Pos.Pose.Y := Coords.Y;
         Pos.Pose.A := 0.0;

         for I in 1 .. Integer (2.0 * Ada.Numerics.Pi / This.FOV) loop
            Append (Nodes, Plan_Node.Create (Pos));
            Pos.Pose.A := Pos.Pose.A + Types.Angle (This.FOV);
            Pos.Assign_Id;
         end loop;
      end;

      return Plan_Node.Create (Plan_Node.And_Node, Nodes);
   end Apply;

end Sancta.Methods.Panorama_Expansion;
