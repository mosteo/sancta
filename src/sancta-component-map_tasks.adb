with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Located_Agent;
with Sancta.Map.Utils,
     Sancta.Tasks.Goto_Pose,
     Sancta.Types;

with Agpl.Random;

use Agpl;

package body Sancta.Component.Map_Tasks is

   ------------
   -- Create --
   ------------

   function Create
     (Config : in Agpl.Xml.Node)
      return Component.Object_Access
   is
   begin
      return new Object (Name'Access, Config);
   end Create;

   -------------------------
   -- Forbidden_Locations --
   -------------------------

   function Forbidden_Locations
     (Map  : Sancta.Map.Object'Class;
      Team : Assignment.Object)
      return Sancta.Map.Location_Lists.List
   is
      use Ac.Lists;
      Locs : Sancta.Map.Location_Lists.List;
      procedure Add_Loc (I : Cursor) is
         Loc : constant Sancta.Map.Location'Class :=
                 Map.Nearest_Location
                   (Located_Agent.Object'Class
                      (Element (I)).Get_Pose);
      begin
         if not Locs.Contains (Loc) then
            Locs.Append (Loc);
         end if;
      end Add_Loc;
   begin
      Team.Get_Agents.Iterate (Add_Loc'Access);
      Log ("Added" & Locs.Length'Img & " forbidden locations",
           Debug, Log_Section);
      return Locs;
   end Forbidden_Locations;


   ------------------
   -- Create_Tasks --
   ------------------

   function Create_Tasks (Amount : Natural;
                          Map    : Sancta.Map.Object'Class;
                          Team   : Assignment.Object;
                          Seed   : Integer := 0)
                          return Tc.Lists.List
   is
      function Is_Valid (This : Sancta.Map.Observation'Class) return Boolean is
      begin
         return This.Is_Traversable;
      end Is_Valid;

      use Sancta.Tasks,
          Sancta.Types;
      Tasks     : Tc.Lists.List;
      Forbidden : constant Sancta.Map.Location_Lists.List :=
                    Forbidden_Locations (Map, Team);
   begin
      Random.Reset (Seed);
      while Natural (Tasks.Length) < Amount loop
         declare
            M : Sancta.Map.Object'Class renames Map;
            L : constant Sancta.Map.Location'Class :=
                  Sancta.Map.Utils.Random_Location (M, Is_Valid'Access);
            P : constant Pose := M.Nearest_Pose (L);
            G : constant Goto_Pose.Object := Goto_Pose.Create (P);

            function Other_At (P : Pose) return Boolean
            is
               use Tc.Lists;
               I : Cursor := Tasks.First;
            begin
               while Has_Element (I) loop
                  if Goto_Pose.Object (Element (I)).Pose = P then
                     return True;
                  end if;
                  Next (I);
               end loop;
               return False;
            end Other_At;
         begin
            if
              (not Other_At (P)) and then
              (not Forbidden.Contains (Map.Nearest_Location (P)))
            then
               Tasks.Append (G);
            end if;
         end;
      end loop;
      Log ("Created" & Amount'Img & " tasks.", Informative, Log_Section);
      return Tasks;
   end Create_Tasks;

   ------------------
   -- Create_Tasks --
   ------------------

   procedure Create_Tasks (This : Object) is
      use Utils;
      Map : constant Sancta.Map.Object_Access :=
        Sancta.Map.Object_Access
                (Types.Map_Data (This.Input (Requires_Map)).Map.Ref);
      Team : Assignment.Object;
   begin
      if This.Provided (Requires_Team) then
         Team := Types.Teams (This.Input (Requires_Team)).Team;
      end if;

      This.Output
        (Provides_Tasks,
         Types.Task_List'
           (Data with
            Create_Tasks
              (Option (This, Option_Amount, 0),
               Map.all,
               Team,
               Option (This, Option_Random_Seed, 0))));
   end Create_Tasks;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Factory.Register (Name, Create'Access);
   end Register;

   ---------
   -- Run --
   ---------

   procedure Run (This : in out Object;
                  Next :    out Ada.Calendar.Time)
   is
      use Ada.Calendar;
   begin
      if This.Created then
         Next := Clock + 1000.0;
      elsif This.Exists (Requires_Map) then
         This.Created := True;
         This.Create_Tasks;
      end if;
   end Run;

end Sancta.Component.Map_Tasks;
