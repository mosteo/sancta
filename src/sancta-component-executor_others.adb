with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Tasks.Explore_Directed_Segment,
     Sancta.Types.Operations;


package body Sancta.Component.Executor_Others is

   type Object_Access is access all Object;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
      This : constant Object_Access := new Object (Name'Access, Config);
   begin
      if This.Exists (Option_Distance) then
         This.Distance :=
           Sancta.Types.Real'Value (This.Option (Option_Distance, ""));
      end if;

      return Component.Object_Access (This);
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This : in out Object;
      Job  : in out Tasks.Object'Class;
      Done :    out Boolean)
   is

      ---------------------------------
      -- Do_Explore_Directed_Segment --
      ---------------------------------

      procedure Do_Explore_Directed_Segment
        (Job  : in out Sancta.Tasks.Object'Class)
      is
         T : Tasks.Explore_Directed_Segment.Object renames
           Tasks.Explore_Directed_Segment.Object (Job);

         use Sancta.Types.Operations;
         use type Sancta.Types.Real;
      begin
         --  First, faultly implementation, will go to the start and then to the
         --  end of the segment, with obstacle avoidance always on.
         pragma Incomplete
           ("When walking the segment we shouldn't deviate from it");

         if T.On_Segment then
            if Distance (Types.Pose (This.Input (Requires_Pose)).Pose,
                         T.Get_To) <= This.Distance then
               Done := True;
            else
               This.Output (Provides_Goal, Types.Pose'(Pose => T.Get_To));
            end if;
         else
            if Distance (Types.Pose (This.Input (Requires_Pose)).Pose,
                         T.Get_From) <= This.Distance then
               T.Set_On_Segment;
            else
               This.Output (Provides_Goal, Types.Pose'(Pose => T.Get_From));
            end if;
         end if;
      end Do_Explore_Directed_Segment;

   begin
      Done := False;
      if Job in Tasks.Explore_Directed_Segment.Object then
         --  Log ("Exploring segment...", Always);
         Do_Explore_Directed_Segment (Job);
      end if;
   end Execute;

end Sancta.Component.Executor_Others;
