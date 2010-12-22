with Agpl.Trace,
     Sancta.Debug2;

use Agpl.Trace;

package body Sancta.Tasks.Navigate_To_Pose is

   ------------
   -- Create --
   ------------

   function Create
     (Goal      : Types.Pose;
      From      : Types.Pose;
      Map       : Sancta.Map.Smart.Object;
      With_Id   : Tasks.Task_Id := Tasks.No_Task)
      return Object
   is
      This : Object :=
               (Goto_Pose.Create (Goal) with
                Goal  => Goal,
                From  => From,
                Map   => Map,
                Path  => Sancta.Map.Location_Lists.Empty_List);
   begin
      Log (Debug2.To_String (From) & " --> " & debug2.to_string (goal),
           Debug, Log_Section);

      This.Path :=
        Map.Ref.Best_Path (Map.Ref.Nearest_Location (From),
                           Map.Ref.Nearest_Location (Goal)).Path;

      if With_Id /= Tasks.No_Task then
         This.Force_Id (With_Id);
      end if;

      return This;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Goal      : Tasks.Goto_Pose.Object'Class;
      From      : Types.Pose;
      Map       : Sancta.Map.Smart.Object;
      Copy_Id   : Boolean := False)
      return Object
   is
   begin
      if Copy_Id then
         return Create (Goal.Get_Pose, From, Map, Goal.Get_Id);
      else
         return Create (Goal.Get_Pose, From, Map, Tasks.No_Task);
      end if;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Path    : Map.Path;
      Goal    : Types.Pose;
      From    : Types.Pose;
      Map     : Sancta.Map.Smart.Object)
      return Object
   is
   begin
      return
        (Goto_Pose.Create (Goal) with
         Goal  => Goal,
         From  => From,
         Map   => Map,
         Path  => Path);
   end Create;

   ------------------
   -- To_Goto_Pose --
   ------------------

   function To_Goto_Pose (This : Object) return Goto_Pose.Object is
      use Map.Paths;
   begin
      if Natural (This.Path.Length) <= 2 then
         --  In same or adjacent cell...
         return Goto_Pose.Create (This.Goal,
                                  This.Use_Angle,
                                  This.Margin_Dist,
                                  This.Margin_Angle);
      else
         --  Center of route cell
         return Goto_Pose.Create
           (This.Map.Ref.Nearest_Pose
              (Element (Next (This.Path.First))),
            This.Use_Angle,
            This.Margin_Dist,
            This.Margin_Angle);
      end if;
   end To_Goto_Pose;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (This : Object) return Map.Path is
   begin
      return This.Path;
   end Get_Path;

   -------------
   -- Get_Map --
   -------------

   function Get_Map (This : Object) return Sancta.Map.Smart.Object is
   begin
      return This.Map;
   end Get_Map;

   ------------
   -- Create --
   ------------

   function Create
     (Pose      : in Types.Pose;
      Use_Angle : in Boolean     := True;
      Margin_D  : in Types.Real  := 0.5;
      Margin_A  : in Types.Angle := 0.25)
      return Object
   is
   begin
      raise Program_Error with "Never should be called";
      pragma Warnings (Off);
      return Create (Pose, Use_Angle, Margin_D, Margin_A);
      pragma Warnings (On);
   end Create;

end Sancta.Tasks.Navigate_To_Pose;
