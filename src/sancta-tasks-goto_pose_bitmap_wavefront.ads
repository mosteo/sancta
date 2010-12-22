with Sancta.Map;
with Sancta.Map.Bitmap;
with Sancta.Map.Bitmap.Smart;
with Sancta.Tasks.Complex_Goto_Pose;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

package Sancta.Tasks.Goto_Pose_Bitmap_Wavefront is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.tasks.goto_pose_bitmap_wavefront";

   type Object (<>) is new Tasks.Complex_Goto_Pose.Object with private;

   not overriding
   function Create (Goal      : Types.Pose;               -- Destination
                    Use_Angle : in Boolean     := True;
                    Margin_D  : in Types.Real  := 0.5;
                    Margin_A  : in Types.Angle := 0.25;
                    From      : Types.Pose;               -- Starting point
                    Map       : Sancta.Map.Bitmap.Smart.Object; -- Where you are
                    With_Id   : Tasks.Task_Id := Tasks.No_Task)
                    return      Object;
   --  It is assumed that the 0, 0 grid cell is aligned with the axis.
   --  i.e. 0, 0 is in the corner of the 0, 0 cell.
   --  Also the gridmap must respect the Y ordering (growing up)
   --  When With_Id = No_Task, a new unique one is used. Else it is copied.

   not overriding
   function Create (Goal      : Tasks.Goto_Pose.Object'Class;
                    From      : Types.Pose;               -- Starting point
                    Map       : Sancta.Map.Bitmap.Smart.Object; -- Where you are
                    Copy_Id   : Boolean := False)
                    return      Object;
   --  Shorthand for the previous one

   not overriding
   function Create (Path    : Map.Path;
                    Goal    : Types.Pose;
                    From    : Types.Pose;
                    Map     : Sancta.Map.Bitmap.Smart.Object) return Object;
   --  Create with explicitly given path

   --  overriding (bug? it overrides from the interface)
   function To_Goto_Pose (This : Object) return Goto_Pose.Object;

   function Get_Route (This : Object) return Sancta.Map.Location_Lists.List;
   --  O (1)

--     function Get_Route (This : Object) return Containers.Pose_Vectors.Object;
   --  O (n)

   function Get_Alternate_Path (This : Map.Bitmap.Object;
                                Ini,
                                Fin  : Types.Pose;
                                Path : Map.Path)
                                return Map.Path;

   function Get_Map (This : Object) return Sancta.Map.Bitmap.Smart.Object;

private

   type Object is new Tasks.Complex_Goto_Pose.Object
   with record
      Goal,
      From  : Types.Pose;
      Map   : Sancta.Map.Bitmap.Smart.Object;
      Route : Sancta.Map.Path;
   end record;

   overriding
   function Create (Pose      : in Types.Pose;
                    Use_Angle : in Boolean     := True;
                    Margin_D  : in Types.Real  := 0.5;
                    Margin_A  : in Types.Angle := 0.25) return Object;
   --  just a rename

   function To_Grid (This : Object;
                     Pose : Types.Pose) return Map.Bitmap.Bit_Location;

   function To_Pose (This : Object;
                     Loc  : Map.Bitmap.Bit_Location) return Types.Pose;

end Sancta.Tasks.Goto_Pose_Bitmap_Wavefront;
