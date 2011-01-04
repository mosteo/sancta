with Agpl.Containers.Graphs,
     Agpl.Containers.Graphs.Adjacency,
     Agpl.Containers.Graphs.Algorithms,
     Sancta.Ctree.Utils,
     Sancta.Ctree.Robot,
     Sancta.Agent.Utils,
     Sancta.Cost_Matrix,
     Sancta.Debug2,
     Sancta.Tasks.Positioned,
     Sancta.Tasks.Utils;

package body Sancta.Ctree.Tree_Navigator.Bitmap_Ordered is

   use type Types.Real;

   -----------
   -- Order --
   -----------

   function Order (This : Object) return Tc.Lists.List is
   begin
      return This.Get_Tasks;
   end Order;

   ------------
   -- Prefix --
   ------------

   function Prefix (M        : Map.Object'Class;
                    Ini, Fin : Types.Pose)
                    return     Map.Path
   is
      P : constant Map.Path := M.Best_Path (Ini, Fin).Path;
   begin
      return P;
   end Prefix;

   ---------------------
   -- Create_With_Mst --
   ---------------------

   function Create_With_Mst
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class
   is
      use Agent.Utils;
      package Gra is new Agpl.Containers.Graphs (Tasks.Task_Id,
                                                 Tasks.No_Task,
                                                 Sancta.Costs,
                                                 Sancta.Infinite,
                                                 Sancta.Tasks."<",
                                                 Sancta.Tasks.Task_Id'Image,
                                                 Sancta."<",
                                                 Sancta.Costs'Image);
      package Adj is new Gra.Adjacency;
      package Alg is new Gra.Algorithms (0.0, Infinite, "+");
      use type Tasks.Task_Id;

      Bot  : constant Robot.Object'Class := Robot.Create ("mst", Head);
      Cm   : constant Cost_Matrix.Object :=
               Cost_Matrix.Create_With_Start (+Bot, Jobs);
      G    : Adj.Graph := Adj.Create (Natural (Jobs.Length) + 1);

      This : Object;

      Base_Id : constant Tasks.Task_Id := 1;
      pragma Assert (Base_Id /= Tasks.No_Task);

      procedure Create_Graph is
         use type Adj.Vertex_Index;
         Pos : Adj.Vertex_Index := 2;
         procedure Create_Vertices (I : Tc.Lists.Cursor) is
         begin
            G.Insert (Pos, Tc.Lists.Element (I).Get_Id);
            Log ("Creating vertex" & Pos'Img &
                 " for task" & Tc.Lists.Element (I).Get_Id'Img &
                 ": " & Tc.Lists.Element (I).To_String,
                 Debug, Log_Section);
            Pos := Pos + 1;
         end Create_Vertices;
         Pos_Ini : Adj.Vertex_Index := 2;
         use Tc.Lists;
         procedure Create_Edges (I : Cursor) is
            Pos_Fin : Adj.Vertex_Index := 2;
            procedure Create_Edge (J : Cursor) is
            begin
               if Pos_Ini /= Pos_Fin then
                  G.Insert
                    (Pos_Ini,
                     Pos_Fin,
                     Cm.Get_Cost
                       (Bot.Get_Name, Element (I).Get_Id, Element (J).Get_Id),
                     Directed => False);
                  Log ("Creating edge" & Element (I).Get_Id'Img & " --" &
                       Element (J).Get_Id'Img & " with cost" &
                       Cm.Get_Cost
                         (Bot.Get_Name, Element (I).Get_Id, Element (J).Get_Id)'Img,
                       Debug, Log_Section);
               end if;
               Pos_Fin := Pos_Fin + 1;
            end Create_Edge;
         begin
            G.Insert
              (1,
               Pos_Ini,
               Cm.Get_Cost (Bot.Get_Name, Tasks.No_task, Element (I).Get_Id),
               Directed => False);
            Log ("Creating edge 00 --" &
                 Element (I).Get_Id'Img & " with cost" &
                 Cm.Get_Cost
                   (Bot.Get_Name, Tasks.No_Task, Element (I).Get_Id)'Img,
                 Debug, Log_Section);
            Jobs.Iterate (Create_Edge'Access);
            Pos_Ini := Pos_Ini + 1;
         end Create_Edges;
      begin
         G.Insert (1, Base_Id);
         Jobs.Iterate (Create_Vertices'Access);
         Jobs.Iterate (Create_Edges'Access);
      end Create_Graph;

   begin
      Log ("MST: Creating graph...", Debug, Log_Section);
      Log ("MST: Base at " & Debug2.To_String (Bot.Get_Pose), Debug, Log_Section);
      Create_Graph;
--      G.Print;

      declare
         use type Adj.Vertex_Index, Adj.Vertex_Cursor'Class;
         P : constant Adj.Graph :=
           Adj.Graph (Alg.Prim (G, G.Vertex (1), Lim));

         function Best_Path (Prev, Curr : Tasks.Task_Id) return Map.Path is
            function Find
              (L : Tc.Lists.List; Id : Tasks.Task_Id)
               return Sancta.Tasks.Object'Class renames Tasks.Utils.Find;
            Fin : constant Types.Pose :=
              Tasks.Positioned.Object'Class (Find (Jobs, Curr)).Pose;
            Ini : Types.Pose;
         begin
            if Prev = Base_Id then
               Ini := Head;
            else
               Ini := Tasks.Positioned.Object'Class (Find (Jobs, Prev)).Pose;
            end if;
            return M.Best_Path (Ini, Fin).Path;
         end Best_Path;

         --  Recursive building of paths
         procedure Build_Path (Prev   : Adj.Vertex_Index;
                               Job    : Adj.Vertex_Index;
                               Prefix : Map.Path)
         is
            Full_Path : Map.Path := Prefix;
            Next      : constant Gra.Edge_Vector'Class :=
              P.Vertex (Job).Incident;
            Fin_Id    : constant Tasks.Task_Id := P.Vertex (Job).Element;
         begin
            Log ("Entering link " & P.Vertex (Prev).Element'Img & " --" &
                 P.Vertex (Job).Element'Img, Debug, Log_Section);
            if Job /= Prev then
               declare
                  New_Part : constant Map.Path :=
                    Best_Path (P.Vertex (Prev).Element,
                               P.Vertex (Job).Element);
               begin
                  if not Full_Path.Is_Empty then
                     Full_Path.Delete_Last;
                  end if;
                  Map.Append (Full_Path, New_Part);
                  This.Set_Branch (Fin_Id, Full_Path);
                  Log ("Setting path for task" & Fin_Id'Img,
                       Debug, Log_Section);
               end;
            else
               Full_Path := Bitmap_Ordered.Prefix (M, Base, Head);
            end if;
            Log ("Checking" & Next.Length'Img & " neighbors of" & Job'Img,
                 Debug, Log_Section);
            for I in Next.First_Index .. Next.Last_Index loop
               if
                 Adj.Vertex_Cursor'Class (Next.Element (I).First).Index = Job and then
                 Adj.Vertex_Cursor'Class (Next.Element (I).Last).Index /= Prev
               then
                  Log ("Adding edge: " & Next.Element (I).Image,
                       Debug, Log_Section);
                  Build_Path (Job,
                              Adj.Vertex_Cursor (Next.Element (I).Last).Index,
                              Full_Path);
               end if;
            end loop;
         end Build_Path;
      begin
--         P.Print;
         Log ("MST: Building paths...", Debug, Log_Section);
         Build_Path (Adj.Vertex_Index'First,
                     Adj.Vertex_Index'First,
                     Map.Paths.Empty_List);
      end;

      Log ("MST: Depth-first-ordering...", Debug, Log_Section);
      This.Set_Order (Ctree.Utils.Depth_Closest_First (This, Jobs));

      Log ("MST: Removing loops...", Debug, Log_Section);
      This.Remove_Loops;

      Log ("MST: Done.", Debug, Log_Section);
      return This;
   end Create_With_Mst;

   ----------------------
   -- Create_With_Ctsp --
   ----------------------

   function Create_With_Ctsp
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class
   is
      pragma Unreferenced (Head, Lim);
   begin
      return Object'
        (Bitmap.Object
           (Bitmap.Create_With_Oca_A_Oca (Base, Jobs, M)) with null record);
   end Create_With_Ctsp;

   -------------------------
   -- Create_With_Steiner --
   -------------------------

   function Create_With_Steiner
     (Base : Types.Pose;
      Head : Types.Pose;
      Jobs : Tc.Lists.List;
      M    : Map.Object'Class;
      Lim  : Costs) return Object'Class
   is
      Mst : Object'Class := Create_With_Mst (Base, Head, Jobs, M, Lim);
   begin
      Mst.Set_Order (Ctree.Utils.Depth_Closest_First (Mst, Jobs));
      Mst := Object'Class (Mst.Steiner_Heuristic_Closest (M));
      Mst.Remove_Loops;

      return Mst;
   end Create_With_Steiner;

   ------------------------
   -- Available_Creators --
   ------------------------

   function Available_Creators (C : Creators) return Creator_Function is
   begin
      case C is
         when Mst     => return Create_With_Mst'Access;
         when Ctsp    => return Create_With_Ctsp'Access;
         when Steiner => return Create_With_Steiner'Access;
      end case;
   end Available_Creators;

   -------------------------------
   -- Steiner_Heuristic_Closest --
   -------------------------------

   function Steiner_Heuristic_Closest
     (This : Object'Class;
      M    : Map.Object'Class) return Object
   is
      Result  :          Object;
      Jobs    : constant Tc.Lists.List := This.Order;
   begin
      Result.Set_Tasks (Jobs);

      if not Jobs.Is_Empty then
         Result.Set_Branch
           (Jobs.First_Element.Get_Id, This.Branch
              (Jobs.First_Element));
         declare
            Prev_Branch : Map.Path := This.Branch (Jobs.First_Element);
            I           : Tc.Lists.Cursor := Tc.Lists.Next (Jobs.First);
            use Tc.Lists;
         begin
            while Has_Element (I) loop
               declare
                  Job : Tasks.Object renames Tasks.Object (Element (I));
                  --  Next target

                  Next_Loc : constant Map.Location'Class :=
                               This.Branch (Job).Last_Element;
                  --  Next target location to visit

                  Near_Loc : constant Map.Location'Class :=
                               M.Nearest_Location_In_Path
                                 (This.Branch (Job).Last_Element,
                                  Prev_Branch);
                  --  Nearest location in previous branch

                  New_Branch : Map.Path;
               begin
                  --  Replace with new one:
                  New_Branch := Map.Prefix (Prev_Branch, Near_Loc);
                  Map.Append
                    (New_Branch, M.Best_Path (Near_Loc, Next_Loc).Path);
                  Result.Set_Branch (Job.Get_Id, New_Branch);
                  Prev_Branch := New_Branch;
               end;
               Next (I);
            end loop;
         end;
      end if;

      Result.Remove_Loops;

      return Result;
   end Steiner_Heuristic_Closest;

end Sancta.Ctree.Tree_Navigator.Bitmap_Ordered;
