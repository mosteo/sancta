with Sancta.Ctree.Robot,
     Sancta.Agent.Utils,
--     Sancta.Assigner.Mtsp_Concorde,
     Sancta.Cost_Matrix,
     Sancta.Debug2,
     Sancta.Map.Utils,
     Sancta.Tasks.Positioned;

use Sancta;

package body Sancta.Ctree.Tree_Navigator.Bitmap is

   ------------
   -- Branch --
   ------------

   function Branch
     (This : Object;
      Goal : Tasks.Object'Class)
      return Map.Path
   is
   begin
      return This.Branches.Element (Goal.Get_Id);
   exception
      when others =>
         Log ("Asked for non-existant branch:" & Goal.Get_Id'Img,
              Warning, Log_Section);
         raise;
   end Branch;

   ---------------
   -- Get_Tasks --
   ---------------

   function Get_Tasks (This : Object) return Tc.Lists.List is
   begin
      return This.Jobs;
   end Get_Tasks;

   ---------------
   -- Set_Tasks --
   ---------------

   procedure Set_Tasks (This : in out Object; Jobs : Tc.Lists.List) is
   begin
      This.Jobs := Jobs;
   end Set_Tasks;

   ----------------
   -- Set_Branch --
   ----------------

   procedure Set_Branch
     (This           : in out Object;
      Goal           :        Tasks.Task_Id;
      Path_From_Base :        Map.Path)
   is
      function Fix return Map.Path is
         pragma Damn_To_Hell_This_Trick;
         Fixed : Map.Path := Robot.Get_Prefix;
      begin
         Map.Append (Fixed, Path_From_Base);
         return Map.Utils.Remove_Loops (Fixed);
      end Fix;
   begin
      This.Branches.Insert (Goal, Fix);
   end Set_Branch;

   -----------------------
   -- Set_Branch_Bypass --
   -----------------------

   procedure Set_Branch_Bypass
     (This           : in out Object;
      Goal           :        Tasks.Task_Id;
      Path_From_Base :        Map.Path)
   is
      Fixed : Map.Path := Robot.Get_Prefix;
   begin
      Map.Append (Fixed, Path_From_Base);
      This.Branches.Include (Goal, Fixed);
   end Set_Branch_Bypass;

   --------------------------------
   -- Create_With_Shortest_Paths --
   --------------------------------

   function Create_With_Shortest_Paths
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class
   is
      This : Object;
      use Tc.Lists;
      procedure Create_With_Shortest_Path (I : Cursor) is
         Job : Tasks.Positioned.Object renames
           Tasks.Positioned.Object (Element (I));
      begin
         Log ("Creating branch for " & Job.Image, Debug, Log_Section);
         Log ("Base is at " & Debug2.To_String (Base), Debug, Log_Section);
         This.Set_Branch
           (Job.Get_Id,
            M.Best_Path
              (M.Nearest_Location (Base),
               M.Nearest_Location (Job.Pose)).Path);
      end Create_With_Shortest_Path;
   begin
      Log ("Creating tree with ShrPath", Debug, Log_Section);
      This.Jobs := Jobs;
      Jobs.Iterate (Create_With_Shortest_Path'Access);

      return This;
   end Create_With_Shortest_Paths;

   --------------------------
   -- Create_With_TSP_Plan --
   --------------------------

--     function Create_With_TSP_Plan
--       (Base : Types.Pose;
--        Jobs : Tc.Lists.List;
--        M    : Map.Object'Class) return Object'Class
--     is
--        use Agent.Utils;
--        Bot  : constant Robot.Object'Class := Robot.Create ("tsp", Base);
--        Cm   : constant Cost_Matrix.Object :=
--                 Cost_Matrix.Create_With_Start (+Bot, Jobs);
--        Opt  : constant Tc.Lists.List :=
--                 Assigner.Mtsp_Concorde.Assign (+Bot, Jobs, Cm).Get_All_Tasks;
--     begin
--        return Create_With_Oca_A_Oca (Base, Opt, M);
--     end Create_With_Tsp_Plan;

   ---------------------------
   -- Create_With_Oca_A_Oca --
   ---------------------------

   function Create_With_Oca_A_Oca
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class
   is
      This        : Object;
      Prev_Branch : Map.Path;

      use Tc.Lists;
      procedure Create (I : Cursor) is
      begin
         if I /= Jobs.First then
            declare
               Job : Tasks.Positioned.Object renames
                 Tasks.Positioned.Object (Element (I));
               Opt : constant Map.Path :=
                 M.Best_Path (Prev_Branch.Last_Element,
                              M.Nearest_Location (Job.Pose)).Path;
               Full_Opt : Map.Path;
            begin
               Full_Opt := Map.Prefix (Prev_Branch, Prev_Branch.Last_Element);
               Map.Append (Full_Opt, Opt);
               Full_Opt := Map.Utils.Remove_Loops (Full_Opt);

               --  Replace with new one:
               Prev_Branch := Full_Opt;
               This.Set_Branch (Job.Get_Id, Full_Opt);
               Log ("Created next branch of length" & Full_Opt.Length'Img,
                    Debug, Log_Section);
            end;
         end if;
      end Create;

      Job : Tasks.Positioned.Object renames
        Tasks.Positioned.Object (Jobs.First_Element);
   begin
      This.Jobs := Jobs;
      --  First task is optimal path
      This.Set_Branch
        (Job.Get_Id,
         M.Best_Path
           (M.Nearest_Location (Base),
            M.Nearest_Location (Job.Pose)).Path);
      Prev_Branch := This.Branch (Job);
      Log ("Created first optimal branch", Debug, Log_Section);

      --  Build the rest
      Jobs.Iterate (Create'Access);

      return This;
   end Create_With_Oca_A_Oca;

   --------------------------------
   -- Create_With_Closest_Branch --
   --------------------------------

   function Create_With_Closest_Branch
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class
   is
      This        : Object;
      Prev_Branch : Map.Path;

      use Tc.Lists;
      procedure Create (I : Cursor) is
      begin
         if I /= Jobs.First then
            declare
               Job : Tasks.Positioned.Object renames
                 Tasks.Positioned.Object (Element (I));
               Loc : constant Map.Location'Class :=
                 M.Nearest_Location_In_Path
                   (M.Nearest_Location (Job.Pose),
                    Prev_Branch);
            begin
               --  Replace with new one:
               Prev_Branch := Map.Prefix (Prev_Branch, Loc);
               Map.Append
                 (Prev_Branch,
                  M.Best_Path (Loc, M.Nearest_Location (Job.Pose)).Path);
               This.Set_Branch (Job.Get_Id, Prev_Branch);
               Log ("Created next branch", Debug, Log_Section);
            end;
         end if;
      end Create;

      Job : Tasks.Positioned.Object renames
        Tasks.Positioned.Object (Jobs.First_Element);
   begin
      This.Jobs := Jobs;
      --  First task is optimal path
      This.Set_Branch
        (Job.Get_Id,
         M.Best_Path
           (M.Nearest_Location (Base),
            M.Nearest_Location (Job.Pose)).Path);
      Prev_Branch := This.Branch (Job);
      Log ("Created first optimal branch", Debug, Log_Section);

      --  Build the rest
      Jobs.Iterate (Create'Access);

      return This;
   end Create_With_Closest_Branch;

   ---------------------------------
   -- Create_With_Cheapest_Branch --
   ---------------------------------

   function Create_With_Cheapest_Branch
     (Base : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class) return Object'Class
   is
      This        : Object;
      Prev_Branch : Map.Path;

      use Tc.Lists;
      procedure Create (I : Cursor) is
      begin
         if I /= Jobs.First then
            declare
               Job : Tasks.Positioned.Object renames
                 Tasks.Positioned.Object (Element (I));
               Loc : constant Map.Location'Class :=
                 M.Nearest_Location_In_Path_From_Path_End
                   (M.Nearest_Location (Job.Pose),
                    Prev_Branch);
            begin
               --  Replace with new one:
               Prev_Branch := Map.Prefix (Prev_Branch, Loc);
               Map.Append
                 (Prev_Branch,
                  M.Best_Path (Loc, M.Nearest_Location (Job.Pose)).Path);
               This.Set_Branch (Job.Get_Id, Prev_Branch);
               Log ("Created next branch", Debug, Log_Section);
            end;
         end if;
      end Create;

      Job : Tasks.Positioned.Object renames
        Tasks.Positioned.Object (Jobs.First_Element);
   begin
      This.Jobs := Jobs;
      --  First task is optimal path
      This.Set_Branch
        (Job.Get_Id,
         M.Best_Path
           (M.Nearest_Location (Base),
            M.Nearest_Location (Job.Pose)).Path);
      Prev_Branch := This.Branch (Job);
      Log ("Created first optimal branch", Debug, Log_Section);

      --  Build the rest
      Jobs.Iterate (Create'Access);

      return This;
   end Create_With_Cheapest_Branch;

   ------------------------
   -- Available_Creators --
   ------------------------

   function Available_Creators (C : Creators) return Creator_Function is
   begin
      case C is
         when Shortest_Paths  => return Create_With_Shortest_Paths'Access;
         when Closest_Branch  => return Create_With_Closest_Branch'Access;
         when Cheapest_Branch => return Create_With_Cheapest_Branch'Access;
--           when Tsp             => return Create_With_Tsp_Plan'Access;
         when Oca_A_Oca       => return Create_With_Oca_A_Oca'Access;
      end case;
   end Available_Creators;

   ------------------
   -- Remove_Loops --
   ------------------

   procedure Remove_Loops (This : in out Object) is
      use Id_Path_Maps;
      procedure Remove_Loop (Id : Tasks.Task_Id; P : in out Sancta.Map.Path) is
         pragma Unreferenced (Id);
      begin
         P := Sancta.Map.Utils.Remove_Loops (P);
      end Remove_Loop;
      procedure Remove_Loops (I : Cursor) is
      begin
         This.Branches.Update_Element (I, Remove_Loop'Access);
      end Remove_Loops;
   begin
      This.Branches.Iterate (Remove_Loops'Access);
   end Remove_Loops;

end Sancta.Ctree.Tree_Navigator.Bitmap;
