with Sancta.Assignment;
with Sancta.Component.Cost_Cache;
with Sancta.Component.Factory;
with Sancta.Component.Helper;
with Sancta.Component.CTypes;

--  with Agpl.Trace; use Agpl.Trace;

package body Sancta.Component.Cost_Updater is

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
      Help : constant Helper.Object := Helper.Create (Config);
      This : constant Object_Access :=
               new Object
                 (Name'Access,
                  Config,
                  Cost_Cache.Cost_Cache (Help.Input (Requires_Cost)).Ptr);
      Thix : Object renames Object (This.all);
   begin
      Thix.Use_External_Tasks :=
        Boolean'Value
          (Thix.Option (Option_Use_Tasks, Boolean'Image (Default_Use_Tasks)));
      Thix.Create_All :=
        Boolean'Value
          (Thix.Option (Option_Create_All, Boolean'Image (Default_Create_All)));

      if Thix.Create_All and then not Thix.Use_External_Tasks then
         Log ("create_all requires use_tasks, implying.", Warning, Log_Section);
         Thix.Use_External_Tasks := True;
      end if;

      if Thix.Exists (Option_Period) then
         Thix.Period.Set_Period (Duration'Value (Thix.Option (Option_Period)));
      end if;

      Thix.Do_Update;
      return This;
   end Create;

   ---------
   -- Run --
   ---------

   procedure Run
     (This : in out Object;
      Next :    out Ada.Calendar.Time)
   is
   begin
      This.Do_Update;
      This.Period.Next (Next);
   end Run;

   ---------------
   -- Do_Update --
   ---------------

   procedure Do_Update (This : in out Object) is
      use type Tc.Lists.List;
      use type Sancta.Agent.Object_Access;
      Bot  : Sancta.Agent.Object_Access;
      Team : Sancta.Assignment.Object;
      Bots : Ac.Lists.List;
   begin
      if This.Provided (Requires_Agent) and then This.Exists (Requires_Agent) then
         Bot := Sancta.Agent.Object_Access
           (Ctypes.Agent (This.Input (Requires_Agent)).Agent);
      end if;
      if This.Provided (Requires_Team) and then This.Exists (Requires_Team) then
         Team := Ctypes.Teams (This.Input (Requires_Team)).Team;
      end if;
      if Bot /= null then
         Team.Set_Agent (Bot.all);
      end if;
      Bots := Team.Get_Agents;

      if Bots.Is_Empty then
         Log ("Skipping cost update, no agents available", Debug, Log_Section);
         return;
      end if;

      This.Cost.Add_Initials (Bots);
      Log ("Initial costs added/updated.", Debug, Log_Section);

      if This.Use_External_Tasks and then This.Exists (Requires_Tasks) then
         declare
            use Ac.Lists;
            procedure For_Agent (I : Cursor) is
               Clon : Agent.Object'Class := Element (I);
            begin
               Clon.Set_Tasks
                 (CTypes.Task_List (This.Input (Requires_Tasks)).Tasks);
               This.Cost.Add_Initials (Clon);
               Log ("External tasks costs added/updated: " & Clon.Get_Name &
                    ", #tasks:" & Clon.Get_Task_Count'Img,
                    Debug, Log_Section);
            end For_Agent;
         begin
            Bots.Iterate (For_Agent'Access);
         end;
      end if;

      if This.Create_All and then
        This.Exists (Requires_Tasks) and then
        This.Tasks /= cTypes.Task_List (This.Input (Requires_Tasks)).Tasks
      then
         This.Tasks := cTypes.Task_List (This.Input (Requires_Tasks)).Tasks;
         Log ("Computing full costs...", Debug, Log_Section);
         pragma Assumption ("All robots are homogeneous!");
         declare
            Clon : Agent.Object'Class := Bots.First_Element;
         begin
            Clon.Set_Tasks (This.Tasks);
            This.Cost.Add_Costs (Clon);
            Log ("Full external tasks costs added/updated: " & Clon.Get_Name &
                 ", #tasks:" & Clon.Get_Task_Count'Img,
                 Debug, Log_Section);
         end;
      end if;
   end Do_Update;

end Sancta.Component.Cost_Updater;
