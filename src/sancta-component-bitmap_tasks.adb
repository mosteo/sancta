with Sancta.Component.Factory,
     Sancta.Component.Types,
     Sancta.Component.Utils,
     Sancta.Located_Agent,
     Sancta.Tasks.Goto_Pose,
     Sancta.Types;

with Agpl.Random;
with Agpl.Strings;
use Agpl;

package body Sancta.Component.Bitmap_Tasks is

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
     (Map  : Sancta.Map.Bitmap.Object'Class;
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

   function Create_Tasks (Amount        : Natural;
                          Map           : Sancta.Map.Bitmap.Object'Class;
                          Team          : Assignment.Object;
                          Seed          : Integer := 0;
                          Cluster_Sigma : Sigma_Range := 0.0;
                          Cluster_Break : Break_Range := 0.9)
                          return Tc.Lists.List
   is
      use Sancta.Tasks,
          Sancta.Types;
      Rnd       : Random.Object;
      Tasks     : Tc.Lists.List;
      Forbidden : constant Sancta.Map.Location_Lists.List :=
                    Forbidden_Locations (Map, Team);

      Attempts  : Natural := 0;
      Passed    : Natural := 0;

      use Agpl.Strings;
   begin
      Rnd.Reset (Seed);

      --  Verify proper repeatability
--        for I in 1 .. 10 loop
--           Log (Rnd.Gaussian (Cluster_Sigma)'Img, Always);
--           Log (Rnd.Uniform'Img, Always);
--        end loop;

      while Natural (Tasks.Length) < Amount loop
         declare
            use type Sancta.Map.Bitmap.Terrains;
            M : Sancta.Map.Bitmap.Object'Class renames Map;
            X : constant Abscissa := Abscissa
              (Rnd.Get_Integer
                 (Integer (M.Get_Data'First (2)),
                  Integer (M.Get_Data'Last (2))));
            Y : constant Ordinate := Ordinate
              (Rnd.Get_Integer
                 (Integer (M.Get_Data'First (1)),
                  Integer (M.Get_Data'Last (1))));
            P : constant Pose := M.To_Pose (M.Loc (X, Y));
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

            function Other_Near (P : Pose) return Boolean
            is
               Nearest : Costs := Infinite;
               Gauss   : constant Float :=
                           abs Rnd.Gaussian (Cluster_Sigma);
               Uniform : constant Float := Rnd.Uniform;
               use Tc.Lists;
               I : Cursor := Tasks.First;
            begin
               Attempts := Attempts + 1;

               while Has_Element (I) loop
                  Nearest := Costs'Min
                    (Nearest,
                     M.Get_Cost_Between
                       (P,
                        Goto_Pose.Object (Element (I)).Pose));
                  Next (I);
               end loop;

               if Float (Nearest) <= Gauss then
                  Passed := Passed + 1;
                  Log (Rpad (Attempts'Img, 5) &
                       " G:" & Nearest'Img & " <=" & Gauss'Img, Always);
               end if;

               declare
                  Updated_Break : constant Float :=
                                Float (Cluster_Break) *
                                         Float (Passed) / Float (Attempts);
               begin
                  if Uniform <= Updated_Break then
                     Log (Rpad (Attempts'Img, 5) &
                          " U:" & Uniform'Img & " <=" & Updated_Break'Img, Always);
                  end if;

                  return Float (Nearest) <= Gauss or else
                         Uniform <= Updated_Break;
               end;
            end Other_Near;
         begin
            if
              M.Get_At (X, Y) = Sancta.Map.Bitmap.Free and then
              (not Other_At (P)) and then
              (not Forbidden.Contains (Map.Nearest_Location (P)))
            then
               if Tasks.Is_Empty or else
                 Cluster_Sigma = 0.0 or else
                 Other_Near (P)
               then
                  Tasks.Append (G);
               end if;
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
      Map : constant Sancta.Map.Bitmap.Object_Access :=
        Sancta.Map.Bitmap.Object_Access
          (Types.Map_Data (This.Input (Requires_Map)).Map.Ref);
   begin
      This.Output
        (Provides_Tasks,
         Types.Task_List'
           (Data with
            Create_Tasks
              (Option (This, Option_Amount, 0),
               Map.all,
               Types.Teams (This.Input (Requires_Team)).Team,
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

end Sancta.Component.Bitmap_Tasks;
