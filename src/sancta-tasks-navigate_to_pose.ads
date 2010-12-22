with Sancta.Map;
with Sancta.Map.Smart;
with Sancta.Tasks.Complex_Goto_Pose;
with Sancta.Tasks.Goto_Pose;
with Sancta.Types;

package Sancta.Tasks.Navigate_To_Pose is

   --  pragma Preelaborate;

   Log_Section : constant String := "sancta.tasks.navigate_to_pose";

   type Object (<>) is new Tasks.Complex_Goto_Pose.Object with private;

   not overriding
   function Create (Goal      : Types.Pose;               -- Destination
                    From      : Types.Pose;               -- Starting point
                    Map       : Sancta.Map.Smart.Object;  -- Where you are
                    With_Id   : Tasks.Task_Id := Tasks.No_Task)
                    return      Object;
   --  When With_Id = No_Task, a new unique one is used. Else it is copied.

   not overriding
   function Create (Goal      : Tasks.Goto_Pose.Object'Class;
                    From      : Types.Pose;               -- Starting point
                    Map       : Sancta.Map.Smart.Object;  -- Where you are
                    Copy_Id   : Boolean := False)
                    return      Object;
   --  Shorthand for the previous one

   not overriding
   function Create (Path    : Map.Path;
                    Goal    : Types.Pose;
                    From    : Types.Pose;
                    Map     : Sancta.Map.Smart.Object) return Object;
   --  Create with explicitly given path

   --  overriding (bug? it overrides from the interface)
   function To_Goto_Pose (This : Object) return Goto_Pose.Object;

   function Get_Path (This : Object) return Map.Path;
   --  O (1)

   function Get_Map (This : Object) return Sancta.Map.Smart.Object;

private

   type Object is new Tasks.Complex_Goto_Pose.Object
   with record
      Goal,
      From  : Types.Pose;
      Map   : Sancta.Map.Smart.Object;
      Path  : Sancta.Map.Path;
   end record;

   overriding
   function Create (Pose      : in Types.Pose;
                    Use_Angle : in Boolean     := True;
                    Margin_D  : in Types.Real  := 0.5;
                    Margin_A  : in Types.Angle := 0.25) return Object;
   --  just a rename

end Sancta.Tasks.Navigate_To_Pose;
