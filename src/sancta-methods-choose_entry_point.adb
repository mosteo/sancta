with Sancta.Tasks.Choose_Entry_Point;
with Sancta.Tasks.Entry_Point;

with Sancta.Tasks.Containers;

package body Sancta.Methods.Choose_Entry_Point is

   -----------
   -- Apply --
   -----------

   function Apply
     (This : in Object;
      That : in Sancta.Tasks.Object'Class)
      return Result
   is
      pragma Unreferenced (This);
   begin
      if not (That in Tasks.Choose_Entry_Point.Object) then
         return null;
      end if;

      --  Create an OR node with the possible entry points.
      declare
         Job      :          Tasks.Choose_Entry_Point.Object renames
           Tasks.Choose_Entry_Point.Object (That);
         Poses    : constant Types.Pose_Array  := Job.Candidates;
         Children :          Sancta.Tasks.Containers.Lists.List;
      begin
         for I in Poses'Range loop
            Children.Append
              (Tasks.Entry_Point.Create (Poses (I), Poses));
         end loop;

         return Sancta.Plan_Node.Create (Sancta.Plan_Node.Or_Node, Children);
      end;
   end Apply;

end Sancta.Methods.Choose_Entry_Point;
